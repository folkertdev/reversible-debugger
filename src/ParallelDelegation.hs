{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module ParallelDelegation where

import Control.Monad.State as State

import LocalType (LocalType, LocalTypeState, Location, Participant, Identifier, Transaction(..))
import qualified LocalType
import qualified GlobalType

import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix

import Types ((|>))


import Semantics (Monitor(..), ExecutionState(..), Program, ProgramF(..), Value(..)
    , Session, List, forward_, emptyQueue, send, receive, terminate, applyFunction, run)


globalType :: GlobalType.GlobalType String
globalType = 
    GlobalType.transaction "A" "B" "thunk1" 
        $ GlobalType.transaction "A" "C" "thunk2" 
        $ GlobalType.transaction "A" "A" "value1" 
        $ GlobalType.transaction "A" "A" "value2" 
        $ GlobalType.end


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections globalType


alice = 
    let
        thunk1 = VFunction "_" (send "A" (VString "Hello 1") terminate) 

        thunk2 = VFunction "_" (send "A" (VString "Hello 2") terminate) 
    in
        run
            [ send "A" thunk1
            , send "A" thunk2
            , receive "A" "x"
            , receive "A" "y"
            ]
        

bob = 
    receive "B" "code" $ applyFunction "code" VUnit

carol = 
    receive "C" "code" $ applyFunction "code" VUnit


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
    in        
    
        ExecutionState
            { variableCount = 0
            , locationCount = 1 
            , applicationCount = 0
            , participants = Map.fromList [ ("A", createMonitor "A"), ("B", createMonitor "B"), ( "C", createMonitor "C") ]
            , locations = 
                [ ("A", alice), ("B", bob), ("C", carol) ]
                    |> Map.fromList
                    |> Map.singleton "Location1" 
            , queue = emptyQueue
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }

steps = 
        ( do
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 
            forward_ "Location1" "C" 

            -- force thunk
            forward_ "Location1" "B" 
            -- perform thunk/send
            forward_ "Location1" "B" 

            -- receive value from B
            forward_ "Location1" "A" 

            -- force thunk
            forward_ "Location1" "C" 
            -- perform thunk/send
            forward_ "Location1" "C" 

            -- receive value from C
            forward_ "Location1" "A" 
        )

            |> flip State.runStateT executionState

