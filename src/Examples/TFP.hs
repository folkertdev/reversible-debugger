{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.TFP where

import Control.Monad.State as State

import LocalType (LocalType, LocalTypeState, Location, Participant, Identifier, Transaction(..))
import qualified LocalType
import qualified GlobalType

import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix
import Data.List as List

import Utils ((|>), List)

import Session (Monitor(..), ExecutionState(..), Session)
import Semantics (forward)
import qualified Semantics
import Program (ProgramF(..), Value(..), Program)
import qualified Queue

import qualified HighLevel as H
import Debug.Trace as Debug

data MyParticipants = A | B | C | V deriving (Show, Eq, Ord, Enum, Bounded)

data MyType
    = Title
    | Price 
    | Share
    | Ok
    | Thunk
    | Address
    | Date
    deriving (Show, Eq, Ord)


globalType :: GlobalType.GlobalType MyParticipants MyType
globalType = GlobalType.globalType $ 
    GlobalType.recurse $ do
        GlobalType.transaction A V Title 
        GlobalType.transactions V [A, B] Price 
        GlobalType.transaction A B Share 
        -- deal or no deal? B can decide and retry when no deal
        -- otherwise finalize the deal
        GlobalType.oneOf B V
            [ (,) "failure" GlobalType.recursionVariable
            , (,) "success" $ do 
                GlobalType.transaction B C Share
                GlobalType.transaction B C Thunk
                GlobalType.transaction B V Address
                GlobalType.transaction V B Date
                GlobalType.end 
            ]


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections $ GlobalType.mapType show globalType


alice = 
    H.recursive $ \self -> do
        let h = VInt 42 
        H.send (VString "Logicomix" )
        p <- H.receive 
        H.send h
        H.offer 
            [ (,) "failure" H.terminate
            , (,) "success" H.terminate
            ]


bob = 
    H.recursive $ \self -> do
        thunk <- 
            H.function $ \_ -> do
                H.send (VString "Lucca, 55100")
                d <- H.receive
                H.terminate

        let ok = VBool True
        price <- H.receive 
        share <- H.receive 
        H.select 
            [ ( "failure", price `H.greaterThan` VInt 42, self )
            , ( "success", VBool True, do 
                H.send share
                H.send thunk 
              )
            ]


carol = 
    H.recursive $ \self -> 
        H.offer 
            [ (,) "failure" self
            , (,) "success" $ do
                h <- H.receive 
                code <- H.receive 
                H.applyFunction code VUnit
            ]

vendor = 
    H.recursive $ \self -> do
        let price title = VInt 42
            date = VString "2018-03-14"

        t <- H.receive 
        H.send (price t) 
        H.send (price t) 

        H.offer 
            [ (,) "failure" $ 
                -- inform the others
                shorter "failure" $ shorter "failure" self
            , (,) "success" $ 
                shorter "success" $ shorter "success" $ do
                    address <- H.receive 
                    H.send date
            ]

shorter label rest = 
    H.select [ (label, VBool True, rest ) ]


constructExecutionState :: List (Participant, H.HighLevelProgram ()) -> ExecutionState Value 
constructExecutionState programs_ = 
    let (participants, programs) = List.unzip programs_
        createMonitor participant = Monitor 
            { _localType = LocalType.Unsynchronized (Fix LocalType.Hole, localTypes Map.! participant)
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
    constructExecutionState $ zip (List.map show [minBound..(maxBound :: MyParticipants)]) [ alice, bob ]

{-
deriveSteps :: Int -> GlobalType.GlobalType String -> Session Value ()
deriveSteps n global = 
    case Debug.traceShowId $ unFix global of 
        GlobalType.Transaction p1 p2 t cont -> do
            Semantics.forwardUntilTypeDecision "Location1" "C"
            Semantics.forwardUntilTypeDecision "Location1" p2
            if n <= 0 then Debug.traceShow ("here!!!!!!****", p1, p2) $ return () else do
                Semantics.forwardUntilTypeDecision "Location1" p1 
                Semantics.forwardTransaction ("Location1", p1) ("Location1", p2) t 
                deriveSteps (n - 1) cont

        GlobalType.End -> 
            return ()

        _ -> error "unsupported for now"

derived = 
    deriveSteps 8 globalType 
        |> flip State.runStateT executionState
-}


steps = 
        ( do
            -- do recursive steps
            forward "l1"
            forward "l1"

            forward "l2"
            forward "l2"

            forward "l3"
            forward "l3"

            forward "l4"
            forward "l4"

            -- initialize thunk
            forward "l2"

            forward "l1"
            forward "l4"

            forward "l4"
            forward "l1"
            forward "l4"
            forward "l2"

            forward "l1"
            forward "l2"

            -- first select/offer
            forward "l2"
            forward "l4"

            -- communicate choice
            forward "l4"
            forward "l1"

            forward "l4"
            forward "l3"

            forward "l2"
            forward "l3"
            
            forward "l2"
            forward "l3"

            -- force thunk
            forward "l3"

            -- evaluate the thunk
            forward "l3"
            forward "l4"

            forward "l4"
            forward "l3"
            {-
            -}
        )
            |> flip State.runStateT executionState
