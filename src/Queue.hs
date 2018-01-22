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

import Types ((|>), List)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty 
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Data.PID (PID)
import Control.Monad.Except as Except
import Data.Actor (Participant)

import Debug.Trace as Debug

import GHC.Generics
import Elm
import Data.Aeson

type ValueType = String

data Item t a = 
    Item 
        { sender :: Participant
        , receiver :: Participant
        , type_ :: t
        , payload :: a 
        } 
        deriving (Show, Eq, Functor, Generic, ElmType, ToJSON, FromJSON)

data Queue t a = 
    Queue 
        { past :: List (QueueHistory t)
        , items :: List (Item t a) 
        }  
        deriving (Show, Eq, Functor, Generic, ElmType, ToJSON, FromJSON)

empty = Queue [] [] 

data QueueHistory t 
    = Added Participant t 
    | Removed Participant t 
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

push :: Participant -> Participant -> t -> a -> Queue t a -> Queue t a
push sender receiver valueType payload queue@Queue{past, items} = 
    Queue { past = Added sender valueType : past, items = items ++ [ Item sender receiver valueType payload ] }

data QueueError 
    = QueueEmpty
    | ItemMismatch { expected :: (Participant, Participant, String) , actual :: (Participant, Participant, String) } 

pop :: (Show t, Eq t) => Participant -> Participant -> t -> Queue t a -> Either QueueError (a, Queue t a)
pop send receive valueType queue@Queue{ past, items } = 
    case items of 
        [] -> 
            Except.throwError QueueEmpty

        (Item{sender, receiver, type_, payload}:xs) -> 
            if send == sender && receive == receiver && type_ == valueType then
                pure ( payload, Queue (Removed receiver valueType : past) xs )

            else
                Except.throwError $ ItemMismatch ( sender, receiver, show type_ ) ( send, receive, show valueType ) 

{-| Undoing a pop, putting the value back into the queue -}
rollPop :: (Show t, Eq t) => Participant -> Participant -> t -> a -> Queue t a -> Either QueueRollError (Queue t a)
rollPop sender receiver valueType payload queue@Queue{past, items} = 
    let value = Item sender receiver valueType payload in
    case queue of 
        Queue ( Removed _ _ : remaining ) [] -> 
            case remaining of 
                [] -> 
                    -- impossible
                    error "there is a Removed, but no corresponding Added"

                (h:hs) -> 
                    pure $ Queue  (h : hs) [value]

        Queue ( Added participant tipe : _ ) [] -> 
            Except.throwError $ InvalidAction "rollpop 1" (show $ Added participant tipe)

        Queue [] [] -> 
            Except.throwError RollPopEmptyQueue 

        Queue (h:hs) items -> 
            case (h, hs ) of 
                ( Removed _ _, [] ) ->
                    -- impossible
                    error "unpopping value that was never added"

                ( Removed participant tipe, remaining ) ->
                    -- only the receiver can undo the read
                    if participant == receiver && tipe == valueType then
                        pure $ Queue remaining (value : items ) 
                    else
                        Except.throwError $ InvalidAction "rollpop 2" (show h)

                ( added, _ ) -> 
                    Except.throwError $ InvalidAction "rollpop 3" (show added)

        Queue [] _ -> 
            error "impossible: no history but there are items"

data QueueRollError
    = RollPushEmptyQueue
    | RollPopEmptyQueue
    | InvalidAction { source :: String, history :: String } 
    deriving (Show, Eq)


rollPush :: (Show a, Show t, Eq t) => Participant -> Participant -> t -> Queue t a -> Either QueueRollError ( Queue t a, Item t a ) 
rollPush sender receiver valueType queue = 
    case queue of 
        Queue [] _ -> 
            Except.throwError RollPushEmptyQueue 

        Queue (h:_) [] -> 
            -- can't rollPush, but can rollPull if there is history
            Except.throwError $ InvalidAction "rollPush 1" (show h)

        Queue (h:history) (i:is) -> 
            -- only the sender can undo a write
            case h of 
                Added participant tipe -> 
                    if participant == sender && tipe == valueType then
                        pure ( Queue history is, i )
                    else
                        Except.throwError $ InvalidAction "rollpush 2" (show h)

                _ -> 
                    Except.throwError $ InvalidAction "rollpush 3" (show h)

             

toHistory :: Queue t a -> List (QueueHistory t)
toHistory = past


hasJustReceived :: Eq t => Participant -> t -> Queue t a -> Bool
hasJustReceived participant valueType queue = 
    case queue of 
        Queue (Removed remover tipe :_) _ ->
            participant == remover && tipe == valueType

        Queue _ _ ->
            False

hasJustSent :: Eq t => Participant -> t -> Queue t a -> Bool
hasJustSent participant valueType queue = 
    case queue of 
        Queue (Added adder tipe:_) _ ->
            participant == adder && tipe == valueType

        Queue _ _ ->
            False

{-
    data Queue t a = Empty (List QueueHistory t) | Queue { items :: NonEmpty (Item a), history :: NonEmpty QueueHistory t } 
    deriving (Show, Eq, Functor) 






empty :: Queue t a
empty = 
    Empty []

push :: PID -> Item a -> Queue t a -> Queue t a
push pid item queue =
    case queue of 
        Queue items history ->
            Queue (items <> pure item) (NonEmpty.cons (Added pid) history)

        Empty history -> 
            Queue (pure item) (Added pid :| history)



pop :: PID -> Queue t a -> Maybe (Item a, Queue t a)
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

revert :: Queue t a -> Maybe (Queue t a, Msg a)
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


unpop :: Item a -> Queue t a -> Queue t a
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


unpush :: Queue t a -> ( Queue t a, Item a ) 
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
followingReceive :: Participant -> Queue t a -> List (QueueHistory t)
followingReceive participant queue =
    queue
        |> toHistory
        |> takeWhile (\h ->
            case h of 
                Removed p _ | p == participant -> True
                _ -> False 
                     )


{-| The instruction immediately following a send on the given thread -} 
followingSend :: Participant -> Queue t a -> List (QueueHistory t)
followingSend participant queue =
    queue
        |> toHistory 
        |> takeWhile (\h -> 
            case h of 
                Added p _ | p == participant -> True
                _ -> False 
                     )

{-| The last N receive instructions (with intermittent sends) on the channel -} 
lastNReceives :: Int -> Queue t a -> List (QueueHistory t)
lastNReceives n queue =
    let 
        go n histories = 
            if n <= 0 then 
                []
            else
                case histories of
                    [] -> []
                    (h@(Added _ _):rest) -> h : go n rest 
                    (h@(Removed _ _)  :rest) -> h : go (n - 1) rest 

    in
        queue
            |> toHistory
            |> go n


{-| The last N send instructions (with intermittent receives) on the channel -} 
lastNSends :: Int -> Queue t a -> List (QueueHistory t)
lastNSends n queue =
    let
        go n histories = 
            if n <= 0 then 
                []
            else
                case histories of
                    [] -> []
                    (h@(Removed _ _):rest) -> h : go n rest 
                    (h@(Added _ _)  :rest) -> h : go (n - 1) rest 
    in
        queue
            |> toHistory
            |> go n
