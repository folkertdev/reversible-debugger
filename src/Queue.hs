{-# LANGUAGE DeriveFunctor, NamedFieldPuns, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}
module Queue 
    ( Queue
    , empty
    , push
    , pop
    , QueueHistory(..)
    , QueueRollError(..)
    --, Msg(..)
    --, revert
    --, hasJustSent
    --, hasJustReceived
    , rollPush
    , rollPop
 --   , unpush
--    , unpop
    , followingSend
    , followingReceive
    , lastNReceives
    , lastNSends
    , Item
    ) where

import Types ((|>), List, Identifier)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty 
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.PID (PID)
import Control.Monad.Except as Except

import Debug.Trace as Debug

import GHC.Generics
import Elm

type ValueType = String

data Item a = Item { sender :: Identifier, receiver :: Identifier, type_ :: ValueType, payload :: a } deriving (Show, Eq, Functor, Generic, ElmType)

data Queue a = Queue { past :: List QueueHistory, items :: List (Item a) }  deriving (Show, Eq, Functor, Generic, ElmType)

empty = Queue [] [] 

data QueueHistory = Added PID  | Removed PID deriving (Show, Eq, Generic, ElmType)

push :: PID -> Identifier -> Identifier -> ValueType -> a -> Queue a -> Queue a
push pid sender receiver valueType payload queue@Queue{past, items} = 
    Queue { past = Added pid : past, items = items ++ [ Item sender receiver valueType payload ] }

data QueueError 
    = QueueEmpty
    | ItemMismatch { expected :: (Identifier, Identifier, ValueType) , actual :: (Identifier, Identifier, ValueType) } 

pop :: PID -> Identifier -> Identifier -> ValueType -> Queue a -> Either QueueError (a, Queue a)
pop pid send receive valueType queue@Queue{ past, items } = 
    case items of 
        [] -> 
            Except.throwError QueueEmpty

        (Item{sender, receiver, type_, payload}:xs) -> 
            if send == sender && receive == receiver && type_ == valueType then
                pure ( payload, Queue (Removed pid : past) xs )

            else
                Except.throwError $ ItemMismatch ( sender, receiver, type_ ) ( send, receive, valueType ) 

rollPop :: PID -> Identifier -> Identifier -> ValueType -> a -> Queue a -> Either QueueRollError (Queue a)
rollPop thread sender receiver valueType payload queue@Queue{past, items} = 
    let value = Item sender receiver valueType payload in
    case queue of 
        Queue ( Removed pid : remaining ) [] -> 
            case remaining of 
                [] -> 
                    -- impossible
                    error "there is a Removed, but no corresponding Added"

                (h:hs) -> 
                    pure $ Queue  (h : hs) [value]

        Queue ( Added  pid : _ ) [] -> 
            Except.throwError $ InvalidAction (Added pid)

        Queue [] [] -> 
            Except.throwError RollPopEmptyQueue 

        Queue (h:hs) items -> 
            case (h, hs ) of 
                ( Removed pid, [] ) ->
                    -- impossible
                    error "unpopping value that was never added"

                ( Removed pid, remaining ) ->
                    if thread == pid then
                        pure $ Queue remaining (value : items ) 
                    else
                        Except.throwError $ InvalidAction h

                ( Added pid, _ ) -> 
                    Except.throwError $ InvalidAction (Added pid)

        Queue [] _ -> 
            error "impossible: no history but there are items"

data QueueRollError
    = RollPushEmptyQueue
    | RollPopEmptyQueue
    | InvalidAction { expected :: QueueHistory } 
    deriving (Show, Eq)


rollPush :: Show a => PID -> Identifier -> Identifier -> Queue a -> Either QueueRollError ( Queue a, Item a ) 
rollPush thread sender receiver queue = 
    case queue of 
        Queue [] _ -> 
            Except.throwError RollPushEmptyQueue 

        Queue (h:_) [] -> 
            -- can't rollPush, but can rollPull if there is history
            Except.throwError $ InvalidAction h

        Queue (h:history) (i:is) -> 
            case h of 
                Added pid -> 
                    if thread == pid then
                        pure ( Queue history is, i )
                    else
                        Except.throwError $ InvalidAction h

                _ -> 
                    Except.throwError $ InvalidAction h

             

toHistory :: Queue a -> List QueueHistory
toHistory = past


hasJustReceived :: PID -> Queue a -> Bool
hasJustReceived pid queue = 
    case queue of 
        Queue (Removed remover:_) _ ->
            pid == remover

        Queue _ _ ->
            False

hasJustSent :: PID -> Queue a -> Bool
hasJustSent pid queue = 
    case queue of 
        Queue (Added adder:_) _ ->
            pid == adder

        Queue _ _ ->
            False

{-
data Queue a = Empty (List QueueHistory) | Queue { items :: NonEmpty (Item a), history :: NonEmpty QueueHistory } 
    deriving (Show, Eq, Functor) 






empty :: Queue a
empty = 
    Empty []

push :: PID -> Item a -> Queue a -> Queue a
push pid item queue =
    case queue of 
        Queue items history ->
            Queue (items <> pure item) (NonEmpty.cons (Added pid) history)

        Empty history -> 
            Queue (pure item) (Added pid :| history)



pop :: PID -> Queue a -> Maybe (Item a, Queue a)
pop pid queue =  
    case queue of
        Empty history ->
            Nothing

        Queue items history -> 
            case NonEmpty.uncons items of 
                ( x, Nothing ) -> 
                    Just (x, Empty (Removed pid : NonEmpty.toList history))
                ( x, Just xs ) -> 
                    Just (x, Queue xs (NonEmpty.cons (Removed pid) history)) 


data Msg a = RevertSendFrom PID a | RevertReceiveTo PID 

revert :: Queue a -> Maybe (Queue a, Msg a)
revert queue = 
    case queue of 
        Empty history -> 
            Nothing
    
        Queue items history -> 
            let 
                ( top :| historyList ) = history 

                newHistory = NonEmpty.nonEmpty historyList

                ( topValue, newItems ) = NonEmpty.uncons items
            in
                case top of 
                    Added pid -> 
                        Just ( fromMaybe (Empty historyList) $ Queue <$> newItems <*> newHistory, RevertSendFrom pid topValue )

                    Removed pid ->
                        Just ( fromMaybe (Empty historyList) $ Queue items <$> newHistory, RevertReceiveTo pid )


unpop :: Item a -> Queue a -> Queue a
unpop value queue = 
    case queue of 
        Empty ( Removed pid : remaining ) -> 
            case remaining of 
                [] -> 
                    error "there is a Removed, but no corresponding Added"

                (h:hs) -> 
                    Queue (pure value) (h :| hs)

        Empty ( Added  pid : _ ) -> 
            error "unpopping a stream that needs to unpush first"

        Empty [] -> 
            error "unpopping empty stream"

        Queue items history -> 
            case NonEmpty.uncons history of
                ( Removed pid, Just remaining ) ->
                    Queue (NonEmpty.cons value items) remaining

                ( Removed pid, Nothing ) ->
                    error "unpopping value that was never added"

                ( Added pid, _ ) -> 
                    error "unpopping, but the most recent action as Added"


unpush :: Queue a -> ( Queue a, Item a ) 
unpush queue = 
    case queue of 
        Empty _ -> error "unpushing on empty queue"
        Queue items history -> 
            let
                ( top :| historyList ) = history 

                newHistory = NonEmpty.nonEmpty historyList
            in
            case ( NonEmpty.uncons items, NonEmpty.uncons history ) of
                (( topValue, remainingValues), ( Added pid, remainingHistory )) -> 
                    ( fromMaybe (Empty historyList) $ Queue <$> remainingValues <*> remainingHistory 
                    , topValue 
                    ) 

                _ -> 
                    error "unpushing, but the most recent action was a Removed"

        -}


{-| The instruction immediately following a receive on the given thread -} 
followingReceive :: PID -> Queue a -> List QueueHistory
followingReceive pid queue =
    queue
        |> toHistory
        |> takeWhile (\h -> h /= Removed pid)


{-| The instruction immediately following a send on the given thread -} 
followingSend :: PID -> Queue a -> List QueueHistory
followingSend pid queue =
    queue
        |> toHistory 
        |> takeWhile (\h -> h /= Added pid)

{-| The last N receive instructions (with intermittent sends) on the channel -} 
lastNReceives :: Int -> Queue a -> List QueueHistory
lastNReceives n queue =
    let 
        go n histories = 
            if n <= 0 then 
                []
            else
                case histories of
                    [] -> []
                    (h@(Added _):rest) -> h : go n rest 
                    (h@(Removed _)  :rest) -> h : go (n - 1) rest 

    in
        queue
            |> toHistory
            |> go n


{-| The last N send instructions (with intermittent receives) on the channel -} 
lastNSends :: Int -> Queue a -> List QueueHistory
lastNSends n queue =
    let
        go n histories = 
            if n <= 0 then 
                []
            else
                case histories of
                    [] -> []
                    (h@(Removed _):rest) -> h : go n rest 
                    (h@(Added _)  :rest) -> h : go (n - 1) rest 
    in
        queue
            |> toHistory
            |> go n
