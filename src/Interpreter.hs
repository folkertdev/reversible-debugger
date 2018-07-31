{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Interpreter (constructExecutionState, untilError, initializeProgram, stepForward, stepBackward) where 

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Monad (foldM)

import LocalType (LocalType, Location, Participant, Identifier, Transaction(..))
import qualified LocalType
import qualified TypeContext 
import qualified GlobalType
import GlobalType (GlobalType)

import Data.Map as Map (Map)
import Data.List as List
import qualified Data.Map as Map
import Data.Fix as Fix

import Utils ((|>), List)
import qualified Utils.Maybe as Maybe
import Zipper

import Session (Monitor(..), ExecutionState(..), Session, Error(..))
import qualified Session 
import Semantics (forward) 
import qualified Semantics
import Program (ProgramF(..), Value(..), Program)
import Queue (QueueError(..))
import qualified Queue

import qualified HighLevel as H
import Data.Void (Void)
import Debug.Trace as Debug

import Data.IORef

initializeProgram :: ExecutionState a -> IO (IORef (ExecutionState  a))
initializeProgram = newIORef  

stepForward :: Location -> IORef (ExecutionState Value) -> IO () 
stepForward location stateRef = do 
    state <- readIORef stateRef 

    case Except.runExcept (State.execStateT (Semantics.forward location) state) of 
        Left e -> 
            error (show e)

        Right newState -> do
            print newState
            writeIORef stateRef newState 


stepBackward :: Location -> IORef (ExecutionState Value) -> IO () 
stepBackward location stateRef = do 
    state <- readIORef stateRef 

    case Except.runExcept (State.execStateT (Semantics.backward location) state) of 
        Left e -> 
            error (show e)

        Right newState -> do
            print newState
            writeIORef stateRef newState 

data Progress = Progress | NoProgress deriving (Eq, Show)

round :: List Location -> ExecutionState Value -> Either Error ( List (Location, Progress), ExecutionState Value)
round locations state = 
    foldM helper [] locations 
        |> flip State.runStateT state
        |> Except.runExcept
  where helper :: List (Location, Progress) -> Location -> Session Value (List (Location, Progress))
        helper accum location = do
            state <- State.get

            case Except.runExcept $ State.runStateT (forward location) state of 
                Right ( _, s ) -> do
                    State.put s
                    return $ ( location, Progress ) : accum 

                Left (QueueError origin (InvalidQueueItem message)) -> 
                    -- blocked on receive, try moving others forward
                    return $ ( location, NoProgress ) : accum 

                Left (QueueError origin EmptyQueue) -> 
                    -- blocked on receive, try moving others forward
                    return $ ( location, NoProgress ) : accum 

                Left Terminated -> 
                    -- don't add this location to the accumulator
                    return accum
                            
                Left err -> 
                    -- other errors are raised
                    -- Except.throwError $ Debug.traceShowId err
                    error (show err)


untilError :: ExecutionState Value -> Either Error (ExecutionState Value)
untilError state@ExecutionState{ locations } = 
    helper (Map.keys locations) state
  where helper locations state = do
            ( locationProgress, newState ) <- Interpreter.round locations state

            if null locationProgress then 
                -- no active locations
                Right state

            else if any (\(_, progress) -> progress == Progress) locationProgress then 
                helper (List.map fst locationProgress) newState

            else
               error $ "DEADLOCK\n" ++ show locationProgress 

        


constructExecutionState :: (Ord participant, Show participant, Show tipe) 
                        => GlobalType participant tipe 
                        -> List (Participant, H.HighLevelProgram Void) 
                        -> ExecutionState Value 
constructExecutionState globalType programs_ = 
    let
        localTypes :: Map Identifier (LocalType.LocalType String)
        localTypes = LocalType.projections $ GlobalType.mapType show globalType

        (participants, programs) = List.unzip programs_
        createMonitor participant = Monitor 
            { _localType = TypeContext.Unsynchronized (TypeContext.Hole, localTypes Map.! participant)
            , _store = Map.empty
            , _applicationHistory = Map.empty
            , _recursiveVariableNumber = 0
            , _recursionPoints = []
            , _usedVariables = []
            }

    in
        ExecutionState
            { variableCount = 0
            , locationCount = List.length programs_
            , applicationCount = 0
            , participants = Map.fromList $ List.map (\p -> ( p, createMonitor p)) participants 
            , locations = 
                programs_
                    |> List.map (uncurry H.compile)
                    |> zip3 participants (repeat [])
                    |> zipWith (\i program -> ("l" ++ show i, program)) [1..]
                    |> Map.fromList
            , queue = Queue.empty
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }



