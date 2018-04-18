{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module RecursiveChoice where

import Control.Monad.State as State
import Control.Monad.Except as Except

import LocalType (LocalType, LocalTypeState, Location, Participant, Identifier, Transaction(..))
import qualified LocalType
import qualified GlobalType

import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix

import Types ((|>))

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

import Debug.Trace as Debug
import Semantics (Monitor(..), ExecutionState(..), Program, ProgramF(..), Value(..)
    , Session, List, forward_, emptyQueue, letBinding, send, run, applyFunction)
import qualified Semantics

recursiveGlobalType :: GlobalType.GlobalType String
recursiveGlobalType = 
    GlobalType.recurse $
        GlobalType.transaction "A" "B" "number" $
            GlobalType.oneOf "A" "B" 
                [ GlobalType.recursionVariable 
                , GlobalType.end
                ]

localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections recursiveGlobalType 


alice = 
    let 
        decremented = VIntOperator (VReference "x") Semantics.Add  (VInt (-1))

        thunk = 
            VFunction "x" 
                $ send "A" (VReference "x")
                $ Semantics.offer "A"
                    [  applyFunction "thunk" decremented 
                    , Semantics.terminate
                    ]
    in 
        letBinding "thunk" thunk $ applyFunction "thunk" (VInt 1)


bob = 
    let 
        comparison = VComparison (VReference "received") GT (VInt 0)


        thunk = VFunction "x" 
            $ Semantics.receive "B" "received"
            $ Semantics.select "B"
                [ (comparison, applyFunction "thunk" VUnit) 
                , (VBool True, Semantics.terminate)
                ]
    in 
        letBinding "thunk" thunk $ applyFunction "thunk" VUnit 



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
            , participants = Map.fromList [ ("A", createMonitor "A"), ("B", createMonitor "B") ]
            , locations = 
                [ ("A", alice), ("B", bob) ]
                    |> Map.fromList
                    |> Map.singleton "Location1" 
            , queue = emptyQueue
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }

{-| This program can be forwarded an arbitrary number of times, and still typecheck

The even forwards will force the thunk, odd forwards perform a send
 -}
steps = 
        ( do
            -- round 1
            forward_ "Location1" "A" 
            forward_ "Location1" "A" 
            forward_ "Location1" "A" 

            forward_ "Location1" "B" 
            forward_ "Location1" "B" 
            forward_ "Location1" "B" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 

            -- round 2
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            -- select & offer
            forward_ "Location1" "B" 
            forward_ "Location1" "A" 

            -- terminate
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 
        )
            |> flip State.runStateT executionState



