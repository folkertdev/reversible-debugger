{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.RecursiveChoice where

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
import Debug.Trace as Debug

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

import Session (Monitor(..), ExecutionState(..), Session)
import Semantics (forward)
import qualified Semantics
import Program (ProgramF(..), Value(..), Program, IntOperator(Add))
import qualified Queue

import qualified HighLevel as H
import Data.Void (Void)

data Participants = A | B deriving (Show, Eq, Ord, Enum, Bounded)


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


alice = do
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




bob = do 
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

constructExecutionState :: List (Participant, H.HighLevelProgram Void) -> ExecutionState Value 
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


{-| This program can be forwarded an arbitrary number of times, and still typecheck

The even forwards will force the thunk, odd forwards perform a send
 -}
steps = 
        ( do
            -- define thunks 
            forward "l1"
            forward "l2" 
            -- round 1

            forward "l1"
            forward "l1"

            forward "l2" 
            forward "l2" 

            {-
                forward "l1"
            forward "l2" 

            forward "l2" 
            forward "l1"

            -- round 2
            forward "l1"
            forward "l2" 

            forward "l1"
            forward "l2" 

            -- select & offer
            forward "l2" 
            forward "l1"

            -- terminate
            forward "l1"
            forward "l2" 
            -}
        )
            |> flip State.runStateT executionState


