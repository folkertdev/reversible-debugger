{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types where

import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM, liftM2)
import Control.Applicative (liftA2, ZipList(..)) 

import Data.Traversable (sequence)

import qualified Data.Map as Map
import Data.PID as PID (PID, create, parent) 
import Data.Identifier as Identifier (Identifier, unwrap) 
import Data.Actor as Actor (Participant) 

import GHC.Generics
import Elm
import Data.Aeson 


infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

data Error
    = UndefinedVariable Identifier
    | UndefinedThread PID
    | UndefinedChannel Identifier
    | UndefinedParticipant Participant
-- | TypeError Identifier String Value
    | TypeError Identifier String String
-- | AssertionError BoolExp
    | AssertionError String
    | BlockedOnReceive Participant 
    | RuntimeException String
    | ArgumentMismatch Identifier Int Int
    | SchedulingError ThreadScheduleError 
    deriving (Eq, Generic, ElmType, ToJSON)

data ThreadScheduleError = ThreadScheduleError PID ThreadScheduleErrorCause deriving (Eq, Show, Generic, ElmType, ToJSON)
data ThreadScheduleErrorCause = ThreadIsBlocked | ThreadIsFiltered | ThreadIsFinished | ThreadDoesNotExist | ThreadIsUninitialized | ParticipantHasNoThread Participant | DeadLock deriving (Eq, Show, Generic, ElmType, ToJSON)



instance Show Error where
    show e = 
        case e of
            UndefinedVariable name -> 
                "Variable `" ++ Identifier.unwrap name ++ "` is undefined"

            UndefinedThread name ->
                "Thread `" ++ show name ++ "` is undefined"

            UndefinedChannel name ->
                "Channel `" ++ Identifier.unwrap name ++ "` is undefined"

            UndefinedParticipant participant -> 
                "Participant `" ++ show participant ++ "` is undefined"

            TypeError name expected actual -> 
                "Type mismatch: I expect value `" ++ Identifier.unwrap name ++ "` to be of type " ++ expected ++ ", but it is " ++ show actual 

            AssertionError expression -> 
                "The assertion `" ++ show expression ++ "` evaluated to False"

            BlockedOnReceive name -> 
                "Thread `" ++ show name ++ "` is blocked on a receive"

            RuntimeException message -> 
                "Something went wrong: " ++ message

            ArgumentMismatch name expected actual -> 
                "Function `" ++ Identifier.unwrap name ++ "` expects " ++ show expected ++ " arguments, but got " ++ show actual

            SchedulingError (ThreadScheduleError pid cause) -> 
                "The scheduler encountered a problem with thread `" ++ show pid ++ "`:\n" ++ show cause



type List = []


