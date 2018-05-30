{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.NestedRecursion where

import Control.Monad.State as State
import Control.Monad.Except as Except

import LocalType (LocalType, Location, Participant, Identifier, Transaction(..))
import qualified TypeContext
import qualified LocalType
import qualified GlobalType

import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix
import Data.List as List

import Utils ((|>), List)

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

import Debug.Trace as Debug
import Session (Monitor(..), ExecutionState(..), Session)
import Semantics (forward)
import qualified Semantics
import Program (ProgramF(..), Value(..), Program, IntOperator(Add))
import qualified Queue

import qualified HighLevel as H

data Participants = A | B deriving (Show, Eq, Ord, Enum, Bounded)

recursiveGlobalType :: GlobalType.GlobalType Participants String
recursiveGlobalType = GlobalType.globalType $ 
    GlobalType.recurse $
        GlobalType.recurse $ do
            GlobalType.transaction A B "number"
            GlobalType.oneOf B A
                [ (,) "recurse"  GlobalType.recursionVariable 
                , (,) "recurse-toplevel"  (GlobalType.weakenRecursion GlobalType.recursionVariable)
                , (,) "end" GlobalType.end
                ]


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections recursiveGlobalType 


alice = 
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


bob =  
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



constructExecutionState :: List (Participant, H.HighLevelProgram ()) -> ExecutionState Value 
constructExecutionState programs_ = 
    let (participants, programs) = List.unzip programs_
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

executionState = 
    constructExecutionState $ zip (List.map show [minBound..(maxBound :: Participants)]) [ alice, bob ]

fullRound = do
            -- assignment
            forward "l1" 
            forward "l2" 

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

            forward "l1" 
            forward "l2"

            forward "l2"
            forward "l1" 

            forward "l1" 
            forward "l2"

            forward "l1" 
            forward "l2"

            forward "l1" 
            forward "l2"

            -- round 3
            forward "l1" 
            forward "l2"

            forward "l2"
            forward "l1" 

            -- round 4
            forward "l1" 
            forward "l2"

            forward "l2"
            forward "l1" 

            forward "l1" 
            forward "l2"

            forward "l1" 
            forward "l2"

            forward "l2"
            forward "l1" 

nested = do
            forward "l1" 
            forward "l1" 

            forward "l2"
            forward "l2"

            forward "l2"
            forward "l1" 

unwrap  = do
            forward "l1" 
            forward "l1" 

            forward "l2"
            forward "l2"

{-| This program can be forwarded an arbitrary number of times, and still typecheck

The even forwards will force the thunk, odd forwards perform a send
 -}
steps = 
        ( do
            -- assignment
            forward "l1" 
            forward "l2"

            unwrap

            nested
            nested
            nested
            nested

        )
            |> flip State.runStateT executionState


