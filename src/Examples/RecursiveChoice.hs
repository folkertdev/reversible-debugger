{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.RecursiveChoice where

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
import Debug.Trace as Debug

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

import Session (Monitor(..), ExecutionState(..), Session)
import Semantics (forward_, backward_)
import qualified Semantics
import Program (ProgramF(..), Value(..), Program, IntOperator(Add))
import qualified Queue

import qualified HighLevel as H

data Participants = A | B deriving (Show, Eq, Ord)


recursiveGlobalType :: GlobalType.GlobalType Participants String
recursiveGlobalType = GlobalType.globalType $
    GlobalType.recurse $ do
        GlobalType.transaction A B "number" 
        GlobalType.oneOf B A
            [ (,) "recurse"  GlobalType.recursionVariable 
            , (,) "end" GlobalType.end
            ]


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections recursiveGlobalType 


alice = H.compile "A" $ do 
    thunk <- 
        H.recursiveFunction $ \self x -> do 
            H.send x
            H.offer 
                [ H.option "recurse" $ do 
                    let decremented = VIntOperator x Program.Add  (VInt (-1))
                    H.applyFunction self decremented

                , H.option "end"
                    H.terminate
                ]

    H.applyFunction thunk (VInt 1)




bob = H.compile "B" $ do 
    thunk <- 
        H.recursiveFunction $ \self x -> do 
            received <- H.receive
            H.select 
                [ H.selection "recurse" (H.greaterThan received (VInt 0)) $ 
                    H.applyFunction self VUnit

                , H.selection "end" (VBool True)
                    H.terminate
                ]

    H.applyFunction thunk VUnit


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

{-| This program can be forwarded an arbitrary number of times, and still typecheck

The even forwards will force the thunk, odd forwards perform a send
 -}
steps = 
        ( do
            -- define thunks 
            forward_ "Location1" "A" 
            forward_ "Location1" "B" 
            -- round 1

            forward_ "Location1" "A" 
            forward_ "Location1" "A" 

            forward_ "Location1" "B" 
            forward_ "Location1" "B" 

            {-
            forward_ "Location1" "A" 
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
            -}
        )
            |> flip State.runStateT executionState


