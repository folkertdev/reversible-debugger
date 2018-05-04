{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.NestedRecursion where

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

import Utils ((|>), List)

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

import Debug.Trace as Debug
import Session (Monitor(..), ExecutionState(..), Session)
import Semantics (forward_, backward_)
import qualified Semantics
import Program (ProgramF(..), Value(..), Program, IntOperator(Add))
import qualified Queue

import qualified HighLevel as H


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


alice = H.compile "Location1" "A" $ 
    let 
        decrement v = 
            VIntOperator v Add  (VInt (-1))

        nested outer x = 
            H.recursiveFunction $ \self y -> do
                H.send y
                H.offer 
                    [ H.option "recurse" $ 
                        H.applyFunction self (decrement y)

                    , H.option "recurse-toplevel" $ 
                        H.applyFunction outer (decrement x)

                    , H.option "end"
                        H.terminate
                    ]

        thunk = 
            H.recursiveFunction $ \self x -> do
                f <- nested self x
                H.applyFunction f x
    in do
        f <- thunk 
        H.applyFunction f (VInt 3)


bob = H.compile "Location1" "B" $ 
    let 
        nested outer =
            H.recursiveFunction $ \self _ -> do
                received <- H.receive
                H.select 
                    [ H.selection "recurse" (H.greaterThan received (VInt 0)) $ 
                        H.applyFunction self VUnit

                    , H.selection "recurse-toplevel" (H.equal received (VInt 0)) $ 
                        H.applyFunction outer VUnit

                    , H.selection "end" (H.lessThan received (VInt 0)) 
                        H.terminate
                    ]

        thunk = 
            H.recursiveFunction $ \self x -> do
                f <- nested self
                H.applyFunction f x
    in do
        f <- thunk 
        H.applyFunction f VUnit


executionState = 
    let createMonitor participant = Monitor 
            { _localType = LocalType.Unsynchronized (Fix LocalType.Hole, localTypes Map.! participant)
            , _store = Map.empty
            , _applicationHistory = Map.empty
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
            , queue = Queue.empty 
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

        )
            |> flip State.runStateT executionState


