module Queue ( Queue, empty, singleton, push, pop, revert) where



data Queue a = 
    Queue
        { items :: [a]
        , history :: [QueueHistory a] 
        } 
    deriving (Show, Eq) 


data QueueHistory a = Added a | Removed a deriving (Show, Eq)

empty :: Queue a
empty = 
    Queue [] [] 

singleton :: a -> Queue a
singleton item =
    Queue [item] [Added item] 


push :: a -> Queue a -> Queue a
push item (Queue items history) = 
    Queue (items ++ [item]) (Added item : history)


pop :: Queue a -> Maybe (a, Queue a)
pop (Queue items history) = 
    case items of
        [] -> 
            Nothing
        (x:xs) -> 
            Just (x, Queue xs (Removed x:history))

revert :: Queue a -> Either (Queue a) (Queue a) 
revert queue@(Queue items history) = 
    case history of 
        [] -> 
            Left queue

        (Added v:restOfHistory) -> 
            Right $ Queue (init items) restOfHistory 

        (Removed v:restOfHistory) -> 
            Right $ Queue (v:items) restOfHistory 

