{-# LANGUAGE  NamedFieldPuns #-}
{-# LANGUAGE  FlexibleContexts #-}
module Queue (Queue, empty, push, pop, removeHistory, popHistory, enqueue, enqueueHistory, QueueError(..), remove) where 

import Control.Monad.State as State
import Control.Monad.Except as Except

type List = []
type Participant = String

data Queue a = Queue { history :: List ( Participant, Participant, a), current :: List (Participant, Participant, a) }  deriving (Eq, Show)

data QueueError 
    = EmptyQueue
    | EmptyQueueHistory
    | InvalidQueueItem String
    | InvalidBackwardQueueItem String
    deriving (Show, Eq)

empty = Queue [] [] 

push :: MonadState (Queue value) m => (Participant, Participant, value) -> m ()
push item = 
    State.modify $ enqueue item

unsafePop :: (MonadState (Queue value) m, MonadError QueueError m) => m (Participant, Participant, value)
unsafePop = do
    q <- State.get
    case dequeue q of 
        Nothing -> 
            Except.throwError EmptyQueue

        Just (value, newQueue ) -> do 
            State.put newQueue
            return value

unsafeRemove :: (MonadState (Queue value) m, MonadError QueueError m) => m (Participant, Participant, value)
unsafeRemove = do
    Queue{history, current}  <- State.get
    case current of 
        [] -> 
            Except.throwError EmptyQueue

        (first:rest) -> do 
            State.put  Queue{ history = history, current = rest } 
            return first


ensure condition error = 
    if condition then 
        return () 
    else 
        Except.throwError error

validate errorConstructor (expectedSender, source) (expectedReceiver, target) = do
    ensure (source == expectedSender) $ 
        errorConstructor $            
            "sender mismatch; the type expects " ++ expectedSender ++ " but the queue contains " ++ source  

    ensure (target == expectedReceiver) $
        errorConstructor $
            "receiver mismatch; the type expects " ++ expectedReceiver ++ " but the queue contains " ++ target  

pop :: (MonadState (Queue value) m, MonadError QueueError m) => Participant -> Participant -> m value
pop expectedSender expectedReceiver = do
    ( source, target, payload ) <- unsafePop 
    
    validate InvalidQueueItem (expectedSender, source) (expectedReceiver, target)

    return payload


remove :: (MonadState (Queue value) m, MonadError QueueError m) => Participant -> Participant -> m value
remove expectedSender expectedReceiver = do
    ( source, target, payload ) <- unsafeRemove 
    
    validate InvalidBackwardQueueItem (expectedSender, source) (expectedReceiver, target)

    return payload


removeHistory :: (MonadState (Queue value) m, MonadError QueueError m) => Participant -> Participant -> m value 
removeHistory expectedSender expectedReceiver = do
    payload <- pop expectedSender expectedReceiver 

    queueRemoveHistory_

    return  payload


popHistory :: (MonadState (Queue value) m, MonadError QueueError m) => Participant -> Participant -> m value 
popHistory expectedSender expectedReceiver = do
    ( source, target, payload ) <- popHistory_

    validate InvalidBackwardQueueItem (expectedSender, source) (expectedReceiver, target)

    return  payload




queueRemoveHistory_ :: (MonadState (Queue value) m, MonadError QueueError m) => m () 
queueRemoveHistory_ = do 
    queue@Queue{ history, current } <- State.get
    case history of 
        [] -> 
            Except.throwError EmptyQueueHistory

        (x:xs) ->
            State.put $ queue { history = xs }


popHistory_ :: (MonadState (Queue value) m, MonadError QueueError m) => m (Participant, Participant, value)
popHistory_ = do
    q <- State.get
    case dequeueHistory q of 
        Nothing -> Except.throwError EmptyQueueHistory
        Just (value, newQueue ) -> do 
            State.put newQueue
            return value

enqueue :: ( Participant, Participant, a ) -> Queue a -> Queue a
enqueue item (queue@Queue{ current }) = queue { current = current ++ [ item ] }

enqueueHistory :: ( Participant, Participant, a ) -> Queue a -> Queue a
enqueueHistory item (queue@Queue{ history }) = queue { history = item : history }

dequeue :: Queue a -> Maybe ((Participant, Participant, a), Queue a)
dequeue queue@Queue{ history, current } = 
    case current of 
        [] -> Nothing
        (x:xs) -> 
            Just ( x, queue { current = xs, history = x : history } )

dequeueHistory :: Queue a -> Maybe ((Participant, Participant, a), Queue a)
dequeueHistory queue@Queue{ history, current } = 
    case history of 
        [] -> Nothing
        (x:xs) -> 
            Just ( x, queue { current = x : current, history = xs } )

atIndex :: Int -> List a -> Maybe a
atIndex 0 (x:xs) = Just x
atIndex n (x:xs) = atIndex (n - 1) xs
atIndex _ [] = Nothing 
        

