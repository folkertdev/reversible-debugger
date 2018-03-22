{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Recursive where

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

recursiveLocalType :: LocalType String
recursiveLocalType = 
    let send :: LocalType String
        send = 
            LocalType.send "Recursive" "C" "share" LocalType.recursionVariable
    in
        LocalType.recurse send

{-| A recursive program that keeps on sending

Because haskell is lazy, we could make a program equivalent to an infinite list of sends, but 
such a program is tricky to work with (it cannot be printed for debugging, for instance). 

Instead, a tail-recursive function is used
-}
recursiveProgram = 
    let 
        thunk = VFunction "_" (send "Recursive" (VString "Lucca, 55100") $ applyFunction "v0" VUnit) 
    in
        letBinding "thunk" thunk $ 
            applyFunction "v0" VUnit



executionState = 
    let createMonitor = Monitor 
            { _localType = (Fix LocalType.Hole, recursiveLocalType ) 
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
            , participants = Map.fromList [ ("Recursive", createMonitor ) ]
            , locations = 
                [ ("Recursive", recursiveProgram )  ]
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
            forward_ "Location1" "Recursive" 
            forward_ "Location1" "Recursive" 
            forward_ "Location1" "Recursive" 
            forward_ "Location1" "Recursive" 
            forward_ "Location1" "Recursive" 
        )
            |> flip State.runStateT executionState
