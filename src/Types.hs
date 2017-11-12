module Types where

import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM, liftM2)
import Control.Applicative (liftA2, ZipList(..)) 
import Control.Monad.Free

import Data.Traversable (sequence)


import Control.Concurrent.Chan
import qualified Data.Map as Map
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Data.IORef


import Queue

type PID = List Int

data Error
    = UndefinedVariable Identifier
    | UndefinedThread PID
    | UndefinedChannel Identifier
-- | TypeError Identifier String Value
    | TypeError Identifier String String
-- | AssertionError BoolExp
    | AssertionError String
    | BlockedOnReceive PID 
    | RuntimeException String
    | ArgumentMismatch Identifier Int Int
    | ThreadScheduleError PID ThreadScheduleError 
    deriving (Eq)


data ThreadScheduleError = ThreadIsBlocked | ThreadIsFiltered | ThreadIsFinished | ThreadDoesNotExist deriving (Eq, Show)



instance Show Error where
    show e = 
        case e of
            UndefinedVariable (Identifier name) -> 
                "Variable `" ++ name ++ "` is undefined"

            UndefinedThread name ->
                "Thread `" ++ show name ++ "` is undefined"

            UndefinedChannel (Identifier name) ->
                "Channel `" ++ name ++ "` is undefined"

            TypeError (Identifier name) expected actual -> 
                "Type mismatch: I expect value `" ++ name ++ "` to be of type " ++ expected ++ ", but it is " ++ show actual 

            AssertionError expression -> 
                "The assertion `" ++ show expression ++ "` evaluated to False"

            BlockedOnReceive name -> 
                "Thread `" ++ show name ++ "` is blocked on a receive"

            RuntimeException message -> 
                "Something went wrong: " ++ message

            ArgumentMismatch (Identifier name) expected actual -> 
                "Function `" ++ name ++ "` expects " ++ show expected ++ " arguments, but got " ++ show actual

            ThreadScheduleError pid error -> 
                "The scheduler encountered a problem with thread `" ++ show pid ++ "`:\n" ++ show error



type List = []

newtype Identifier 
    = Identifier String 
    deriving (Eq, Show, Ord)

newtype ThreadName  
    = ThreadName String 
    deriving (Eq, Show, Ord)


