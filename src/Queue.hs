module Queue ( Queue(..), empty, push, pop, QueueHistory(..), QueueHistoryError(..), Msg(..), revert, hasJustSent, hasJustReceived, unpush, unpop, followingSend, followingReceive) where

import Types ((|>), List)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty 
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

type PID  = [Int]

data Queue a = Empty (List QueueHistory) | Queue { items :: NonEmpty a, history :: NonEmpty QueueHistory } 
    deriving (Show, Eq) 

instance Functor Queue where 
    fmap tagger (Empty history) = Empty history 
    fmap tagger (Queue items history) = Queue (fmap tagger items) history


data QueueHistory = Added PID  | Removed PID deriving (Show, Eq)

toHistory :: Queue a -> List QueueHistory
toHistory queue = 
    case queue of 
        Empty hs -> hs
        Queue _ hs -> NonEmpty.toList hs


empty :: Queue a
empty = 
    Empty []

push :: PID -> a -> Queue a -> Queue a
push pid item queue =
    case queue of 
        Queue items history ->
            Queue (items <> pure item) (NonEmpty.cons (Added pid) history)

        Empty history -> 
            Queue (pure item) (Added pid :| history)



pop :: PID -> Queue a -> Maybe (a, Queue a)
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


data QueueHistoryError = QueueEmpty | InvalidThread { expected :: PID, actual :: PID } | InvalidAction deriving (Show) 

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

hasJustReceived :: PID -> Queue a -> Bool
hasJustReceived pid queue = 
    case queue of 
        Queue items history ->
            case NonEmpty.head history of
                Removed remover ->
                    pid == remover

                Added _ -> 
                    False

        Empty (Removed remover : _) -> 
            pid == remover


        Empty (Added _ : _) -> 
            False


hasJustSent :: PID -> Queue a -> Bool
hasJustSent pid queue = 
    case queue of 
        Queue items history ->
            case NonEmpty.head history of
                Removed _ ->
                    False

                Added adder -> 
                    adder == pid

        Empty _ -> 
            False

unpop :: a -> Queue a -> Queue a
unpop value queue = 
    case queue of 
        Empty ( Removed pid : h : hs ) -> 
            Queue (pure value) (h :| hs)

        Empty ( Added  pid : hs ) -> 
            error "unpopping a stream that needs to unpush first"

        Queue items history -> 
            case NonEmpty.uncons history of
                ( Removed pid, Just remaining ) ->
                    Queue (NonEmpty.cons value items) remaining

                ( Removed pid, Nothing ) ->
                    error "unpopping value that was never added"

                ( Added pid, _ ) -> 
                    error "unpopping, but the most recent action as Added"


unpush :: Queue a -> ( Queue a, a ) 
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


{-| The instruction immediately following a receive on the given thread -} 
followingReceive :: PID -> Queue a -> List QueueHistory
followingReceive pid queue =
    queue
        |> toHistory
        |> reverse
        |> takeWhile (\h -> h /= Removed pid)


{-| The instruction immediately following a send on the given thread -} 
followingSend :: PID -> Queue a -> List QueueHistory
followingSend pid queue =
    queue
        |> toHistory 
        |> reverse
        |> takeWhile (\h -> h /= Added pid)
