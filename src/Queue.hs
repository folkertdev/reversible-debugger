{-# LANGUAGE DeriveFunctor, NamedFieldPuns, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Queue 
    ( Queue
    , Transaction(..)
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
    , Item(..)
    , QueueError(ItemMismatch, QueueEmpty)
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

{-| A single transaction -} 
data Transaction t = 
    Transaction 
        { sender :: Participant 
        , receiver :: Participant
        , valueType :: t
        }
    deriving (Show, Eq)

invert :: Transaction t -> Transaction t
invert Transaction{ sender, receiver, valueType } = 
    Transaction 
        { sender = receiver
        , receiver = sender
        , valueType = valueType 
        } 

equalsItem :: Eq t => Transaction t -> Item t a -> Bool
equalsItem (Transaction s1 r1 t1) (Item s2 r2 t2 _) = 
    s1 == s2 && r1 == r2 && t1 == t2

-- FORWARD

push :: Transaction t -> a -> Queue t a -> Queue t a
push Transaction{sender, receiver, valueType} payload queue@Queue{past, items} = 
    Queue { past = Added sender valueType : past, items = items ++ [ Item sender receiver valueType payload ] }

data QueueError 
    = QueueEmpty
    | forall a t. (Show a, Show t) => ItemMismatch { expected :: Transaction t , actual :: Item t a } 
    
deriving instance Show QueueError

pop :: (Show a, Show t, Eq t) => Transaction t -> Queue t a -> Either QueueError (a, Queue t a)
pop transaction@Transaction{sender, receiver, valueType} queue@Queue{ past, items } = 
    case items of 
        [] -> 
            Except.throwError QueueEmpty

        (item@Item{payload}:xs) -> 
            if equalsItem transaction item then
                pure ( payload, Queue (Removed receiver valueType : past) xs )

            else
                Except.throwError $ ItemMismatch transaction item 

{-| Undoing a pop, putting the value back into the queue -}
rollPop :: (Show t, Eq t) => Transaction t -> a -> Queue t a -> Either QueueRollError (Queue t a)
rollPop Transaction{sender, receiver, valueType} payload queue@Queue{past, items} = 
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
    | forall t. Show t => InvalidAction { source :: t, history :: t } 

deriving instance Show QueueRollError


rollPush :: (Show a, Show t, Eq t) => Transaction t -> Queue t a -> Either QueueRollError ( Queue t a, Item t a ) 
rollPush Transaction{sender, receiver, valueType} queue = 
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
