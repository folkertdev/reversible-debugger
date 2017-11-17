module Queue ( Queue, empty, push, pop, tryRevertPush, tryRevertPop, QueueHistory(..), mostRecentAction, QueueHistoryError) where

type PID  = [Int]

data Queue a = 
    Queue
        { items :: [a]
        , history :: [QueueHistory] 
        } 
    deriving (Show, Eq) 


data QueueHistory = Added PID  
                  | Removed PID deriving (Show, Eq)

mostRecentAction :: Queue a -> Maybe QueueHistory
mostRecentAction (Queue _ history) = 
    case history of
        [] -> Nothing
        (x:xs) -> Just x

empty :: Queue a
empty = 
    Queue [] [] 

push :: PID -> a -> Queue a -> Queue a
push pid item (Queue items history) = 
    Queue (items ++ [item]) (Added pid : history)


pop :: PID -> Queue a -> Maybe (a, Queue a)
pop pid (Queue items history) = 
    case items of
        [] -> 
            Nothing
        (x:xs) -> 
            Just (x, Queue xs (Removed pid : history))

data QueueHistoryError = QueueEmpty | InvalidThread { expected :: PID, actual :: PID } | InvalidAction deriving (Show) 

tryRevertPush :: PID -> Queue a -> Either QueueHistoryError (Queue a) 
tryRevertPush pid (Queue items history) =
    case history of
        [] -> 
            Left QueueEmpty

        (Added adder : rest) -> 
            if adder == pid then
                Right $ Queue (drop 1 items) rest 
            else 
                Left InvalidThread{ expected = adder, actual = pid } 
        
        (_:_) -> 
            Left InvalidAction 

tryRevertPop :: PID -> a -> Queue a -> Either QueueHistoryError (Queue a) 
tryRevertPop pid value (Queue items history) = 
    case history of
        [] -> 
            Left QueueEmpty

        (Removed remover : rest) -> 
            if remover == pid then
                Right $ Queue (value : items) rest 
            else 
                Left InvalidThread{ expected = remover, actual = pid } 
        
        (_:_) -> 
            Left InvalidAction 
