{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.TFP where

import Control.Monad.State as State

import LocalType (LocalType, LocalTypeState, Location, Participant, Identifier, Transaction(..))
import qualified LocalType
import qualified GlobalType

import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix

import Utils ((|>))

import Session (Monitor(..), ExecutionState(..), Session)
import Semantics (forward_, backward_)
import qualified Semantics
import Program (ProgramF(..), Value(..), Program)
import qualified Queue

import qualified HighLevel as H
import Debug.Trace as Debug

data MyParticipants = A | B | C | V deriving (Show, Eq, Ord)

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
        GlobalType.transactions B [A, V] Ok 
        GlobalType.transaction B C Share
        GlobalType.transaction B C Thunk
        -- deal or no deal? B can decide and retry when no deal
        -- otherwise finalize the deal
        GlobalType.oneOf B V
            [ (,) "failure" GlobalType.recursionVariable
            , (,) "success" $ do 
                GlobalType.transaction B V Address
                GlobalType.transaction V B Date
                GlobalType.end 
            ]


localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections $ GlobalType.mapType show globalType


alice = H.compile "Location1" "A" $ do 
    let h = VInt 42 
    H.send (VString "Logicomix" )
    p <- H.receive 
    H.send h
    ok <- H.receive 
    H.terminate
            

bob = H.compile "Location1" "B" $ do 
    thunk <- 
        H.function $ \_ -> do
            H.send (VString "Lucca, 55100")
            d <- H.receive
            H.terminate

    let ok = VBool True
    p <- H.receive 
    h <- H.receive 
    H.send ok 
    H.send ok 
    H.send h 
    H.send thunk 


carol = H.compile "Location1" "C" $ do 
    h <- H.receive 
    code <- H.receive 
    H.applyFunction code VUnit

vendor = H.compile "Location1" "V" $ do 
    let price title = VInt 42
        date = VString "2018-03-14"

    t <- H.receive 
    H.send (price t) 
    H.send (price t) 
    ok <- H.receive 
    a <- H.receive 
    H.send date

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
            , participants = Map.fromList [ ("A", createMonitor "A"), ("B", createMonitor "B"), ( "C", createMonitor "C"), ("V", createMonitor "V") ]
            , locations = 
                [ ("A", alice), ("B", bob), ("C", carol), ("V", vendor) ]
                    |> Map.fromList
                    |> Map.singleton "Location1" 
            , queue = Queue.empty
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }

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
            -- initialize thunk
            forward_ "Location1" "B" 

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
