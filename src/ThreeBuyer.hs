{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module ThreeBuyer where

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
    GlobalType.transaction "A" "V" "title" 
        $ GlobalType.transaction "V" "A" "price" 
        $ GlobalType.transaction "V" "B" "price" 
        $ GlobalType.transaction "A" "B" "share" 
        $ GlobalType.transaction "B" "A" "ok" 
        $ GlobalType.transaction "B" "V" "ok" 
        $ GlobalType.transaction "B" "C" "share"
        $ GlobalType.transaction "B" "C" "thunk"
        $ GlobalType.transaction "B" "V" "address"
        $ GlobalType.transaction "V" "B" "date"
        $ GlobalType.end


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections globalType


alice = 
    let h = VInt 42 
    in
        run 
            [ send "A" (VString "Logicomix" )
            , receive "A" "p" 
            , send "A" h
            , receive "A" "ok"
            ]

bob = 
    let
        thunk = VFunction "_" (send "B" (VString "Lucca, 55100") $ receive "B" "d" terminate) 

        ok = VBool True
    in
        run 
            [ receive "B" "p"
            , receive "B" "h"
            , send "B" ok 
            , send "B" ok 
            , send "B" (VReference "h") 
            , send "B" thunk 
            ]

carol = 
    receive "C" "h" $
    receive "C" "code" $
    applyFunction "code" VUnit

vendor = 
    let price title = VInt 42
        date = VString "2018-03-14"
    in
        run
            [ receive "V" "t"
            , send "V" (price (VReference "t")) 
            , send "V" (price (VReference "t")) 
            , receive "V" "ok"
            , receive "V" "a"
            , send "V" date
            ]


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
            , participants = Map.fromList [ ("A", createMonitor "A"), ("B", createMonitor "B"), ( "C", createMonitor "C"), ("V", createMonitor "V") ]
            , locations = 
                [ ("A", alice), ("B", bob), ("C", carol), ("V", vendor) ]
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
            forward_ "Location1" "V" 

            forward_ "Location1" "V" 
            forward_ "Location1" "A" 
            forward_ "Location1" "V" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "B" 
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 
            forward_ "Location1" "V" 

            forward_ "Location1" "B" 
            forward_ "Location1" "C" 
            
            forward_ "Location1" "B" 
            forward_ "Location1" "C" 

            -- force thunk
            forward_ "Location1" "C" 

            -- evaluate the thunk
            forward_ "Location1" "C" 
            forward_ "Location1" "V" 

            forward_ "Location1" "V" 
            forward_ "Location1" "C" 
        )
            |> flip State.runStateT executionState
