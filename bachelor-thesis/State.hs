{-# LANGUAGE InstanceSigs #-}
module State where 

import Control.Monad (ap)
import Control.Monad.Trans.Class

newtype State s a = State { runState :: s -> (a, s) } 

andThen :: State s a -> (a -> State s b) -> State s b
andThen (State first) tagger = 
    State $ \s -> 
        let (value, newState) = first s
            State second = tagger value
        in
            second newState 

new :: a -> State s a
new value = State (\s -> (value, s))

instance Monad (State s) where
    (>>=) = andThen
    return = new

instance Applicative (State s) where
    (<*>) = ap
    pure = return

instance Functor (State s) where
    fmap f (State g) = State $ \s -> let ( value, newState ) = g s in ( f value, newState)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }  

instance MonadTrans (StateT s) where 
    lift :: (Monad m) => m a -> StateT s m a
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)
