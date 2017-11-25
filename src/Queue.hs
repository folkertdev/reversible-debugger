module Queue ( Queue(..), empty, push, pop, QueueHistory(..), QueueHistoryError(..), Msg(..), revert, hasJustSent, hasJustReceived, unpush, unpop, followingSend, followingReceive) where

import Types ((|>), List)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

type NonEmpty = NonEmpty.NonEmpty
type PID  = [Int]

data Queue a = Empty | Queue { items :: NonEmpty a, history :: NonEmpty QueueHistory } 
    deriving (Show, Eq) 

instance Functor Queue where 
    fmap tagger Empty = Empty
    fmap tagger (Queue items history) = Queue (fmap tagger items) history


data QueueHistory = Added PID  | Removed PID deriving (Show, Eq)


empty :: Queue a
empty = 
    Empty

push :: PID -> a -> Queue a -> Queue a
push pid item queue =
    case queue of 
        Queue items history ->
            Queue (items <> pure item) (NonEmpty.cons (Added pid) history)

        Empty -> 
            Queue (pure item) (pure (Added pid))



pop :: PID -> Queue a -> Maybe (a, Queue a)
pop pid queue =  
    case queue of
        Empty ->
            Nothing

        Queue items history -> 
            case NonEmpty.uncons items of 
                ( x, Nothing ) -> 
                    Just (x, Empty )
                ( x, Just xs ) -> 
                    Just (x, Queue xs (NonEmpty.cons (Removed pid) history)) 


data QueueHistoryError = QueueEmpty | InvalidThread { expected :: PID, actual :: PID } | InvalidAction deriving (Show) 

data Msg a = RevertSendFrom PID a | RevertReceiveTo PID 

revert :: Queue a -> Maybe (Queue a, Msg a)
revert queue = 
    case queue of 
        Empty -> 
            Nothing
    
        Queue items history -> 
            let 
                ( top, newHistory ) = NonEmpty.uncons history 

                ( topValue, newItems ) = NonEmpty.uncons items
            in
                case top of 
                    Added pid -> 
                        Just ( fromMaybe Empty $ Queue <$> newItems <*> newHistory, RevertSendFrom pid topValue )

                    Removed pid ->
                        Just ( fromMaybe Empty $ Queue items <$> newHistory, RevertReceiveTo pid )

hasJustReceived :: PID -> Queue a -> Bool
hasJustReceived pid queue = 
    case queue of 
        Queue items history ->
            case NonEmpty.head history of
                Removed remover ->
                    pid == remover

                Added _ -> 
                    False

        Empty -> 
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

        Empty -> 
            False

unpop :: a -> Queue a -> Queue a
unpop value queue = 
    case queue of 
        Empty -> error "unpopping on empty queue"
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
        Empty -> error "unpushing on empty queue"
        Queue items history -> 
            case ( NonEmpty.uncons items, NonEmpty.uncons history ) of
                (( topValue, remainingValues), ( Added pid, remainingHistory )) -> 
                    ( fromMaybe Empty $ Queue <$> remainingValues <*> remainingHistory 
                    , topValue 
                    ) 

                _ -> 
                    error "unpushing, but the most recent action was a Removed"


{-| The instruction immediately following a receive on the given thread -} 
followingReceive :: PID -> Queue a -> List QueueHistory
followingReceive pid queue =
    case queue of
        Empty -> [] 

        Queue items history -> 
            NonEmpty.toList history
                |> reverse
                |> takeWhile (\h -> h /= Removed pid)


{-| The instruction immediately following a send on the given thread -} 
followingSend :: PID -> Queue a -> List QueueHistory
followingSend pid queue =
    case queue of
        Empty -> [] 

        Queue items history -> 
            NonEmpty.toList history
                |> reverse
                |> takeWhile (\h -> h /= Added pid)
