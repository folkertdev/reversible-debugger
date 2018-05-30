{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.ParallelDelegation where

import Control.Monad.State as State

import LocalType (LocalType, LocalTypeState, Location, Participant, Identifier, Transaction(..))
import qualified LocalType
import qualified GlobalType

import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix

import Utils ((|>), List)


import Semantics (Monitor(..), ExecutionState(..), Program, ProgramF(..), Value(..)
    , Session, forward_, emptyQueue) 

import HighLevel (send, receive, terminate, applyFunction, function)
import qualified HighLevel as H

globalType :: GlobalType.GlobalType String
globalType = 
    GlobalType.transaction "A" "B" "thunk1" 
        $ GlobalType.transaction "A" "C" "thunk2" 
        $ GlobalType.transaction "A" "A" "value1" 
        $ GlobalType.transaction "A" "A" "value2" 
        $ GlobalType.end


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections globalType


alice = H.compile "Location1" "A" $ do 
    thunk1 <- function $ \_ -> send (VString "Hello1")
    thunk2 <- function $ \_ -> send (VString "Hello2")

    send thunk1
    send thunk2

    x <- receive 
    y <- receive 

    terminate

bob = H.compile "Location1" "B" $ do 
    code <- receive 
    applyFunction code VUnit

carol = H.compile "Location1" "C" $ do 
    code <- receive 
    applyFunction code VUnit


executionState = 

    let createMonitor participant = Monitor 
            { _localType = (Fix LocalType.Hole, localTypes Map.! participant)
            , _freeVariables = [] 
            , _store = Map.empty
            , _applicationHistory = Map.empty
            , _reversible = False 
            , _recursiveVariableNumber = 0
            , _recursionPoints = []
            }

        loc1 = 
            [ ("A", alice) ]
                |> Map.fromList
        loc2 =
            [ ("B", bob), ("C", carol) ]
                |> Map.fromList

        locations = 
            Map.fromList [ ("Location1", loc1), ("Location2", loc2) ]
    in        
    
        ExecutionState
            { variableCount = 0
            , locationCount = 1 
            , applicationCount = 0
            , participants = Map.fromList [ ("A", createMonitor "A"), ("B", createMonitor "B"), ( "C", createMonitor "C") ]
            , locations = locations
            , queue = emptyQueue
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }

steps = 
        ( do
            -- define thunks
            forward_ "Location1" "A" 
            forward_ "Location1" "A" 


            -- start execution
            forward_ "Location1" "A" 
            forward_ "Location2" "B" 

            forward_ "Location1" "A" 
            forward_ "Location2" "C" 

            -- force thunk
            forward_ "Location2" "B" 
            -- perform thunk/send
            forward_ "Location2" "B" 

            -- receive value from B
            forward_ "Location1" "A" 

            -- force thunk
            forward_ "Location2" "C" 
            -- perform thunk/send
            forward_ "Location2" "C" 

            -- receive value from C
            forward_ "Location1" "A" 
        )

            |> flip State.runStateT executionState

