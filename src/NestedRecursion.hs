{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module NestedRecursion where

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
    , Session, List, forward_, backward_, emptyQueue, letBinding, send, run, applyFunction)
import qualified Semantics



recursiveGlobalType :: GlobalType.GlobalType String
recursiveGlobalType = 
    GlobalType.recurse $
        GlobalType.recurse $
            GlobalType.transaction "A" "B" "number" $
                GlobalType.oneOf "A" "B" 
                    [ (,) "recurse"  GlobalType.recursionVariable 
                    , (,) "recurse-toplevel"  (GlobalType.weakenRecursion GlobalType.recursionVariable)
                    , (,) "end" GlobalType.end
                    ]


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections recursiveGlobalType 


alice = 
    let 
        decremented name = VIntOperator (VReference name) Semantics.Add  (VInt (-1))

        nested = 
            VFunction "y" 
                $ send "A" (VReference "y")
                $ Semantics.offer "A"
                    [ ("recurse", applyFunction "nested" (decremented "y"))
                    , ("recurse-toplevel", applyFunction "thunk" (decremented "x"))
                    , ("end", Semantics.terminate)
                    ]

        thunk = 
                VFunction "x" $ 
                    letBinding "nested" nested $
                    applyFunction "nested" (VReference "x")

    in 
        letBinding "thunk" thunk $ 
            applyFunction "thunk" (VInt 3)


bob = 
    let 
        comparison c = VComparison (VReference "received") c (VInt 0)


        nested = VFunction "_" 
            $ Semantics.receive "B" "received"
            $ Semantics.select "B"
                [ ("recurse",  comparison GT , applyFunction "nested" VUnit) 
                , ("recurse-toplevel", comparison EQ, applyFunction "thunk" VUnit) 
                , ("end", comparison LT, Semantics.terminate)
                ]

        thunk = 
            VFunction "x" $ 
                letBinding "nested" nested $ 
                applyFunction "nested" (VReference "x")
    in 
        letBinding "thunk" thunk $
            applyFunction "thunk" VUnit 



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

fullRound = do
            -- assignment
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            unwrap

            nested
            nested
            nested
            nested

            unwrap

            nested
            nested
            nested

            unwrap 

            nested
            nested

            unwrap 
            nested

            unwrap
            nested

full = do
            fullRound

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "B" 
            forward_ "Location1" "A" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            -- round 3
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "B" 
            forward_ "Location1" "A" 

            -- round 4
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "B" 
            forward_ "Location1" "A" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            forward_ "Location1" "B" 
            forward_ "Location1" "A" 

nested = do
            forward_ "Location1" "A" 
            forward_ "Location1" "A" 

            forward_ "Location1" "B" 
            forward_ "Location1" "B" 

            forward_ "Location1" "B" 
            forward_ "Location1" "A" 

unwrap  = do
            forward_ "Location1" "A" 
            forward_ "Location1" "A" 

            forward_ "Location1" "B" 
            forward_ "Location1" "B" 

{-| This program can be forwarded an arbitrary number of times, and still typecheck

The even forwards will force the thunk, odd forwards perform a send
 -}
steps = 
        ( do
            -- assignment
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 

            unwrap

            nested
            nested
            nested
            nested

            forward_ "Location1" "A" 
            backward_ "Location1" "A" 

        )
            |> flip State.runStateT executionState



