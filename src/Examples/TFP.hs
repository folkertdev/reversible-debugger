{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.TFP where

import Control.Monad.State as State

import LocalType (LocalType, Location, Participant, Identifier, Transaction(..))
import qualified TypeContext
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
import Data.Void (Void)
import qualified Interpreter
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
globalType = 
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

alice :: H.HighLevelProgram a
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
    


bob :: H.HighLevelProgram a
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
                H.terminate
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
                    H.terminate
            ]

shorter label rest = 
    H.select [ (label, VBool True, rest ) ]

executionState = 
    Interpreter.constructExecutionState globalType $
        zip (List.map show [minBound..(maxBound :: MyParticipants)]) [ alice, bob, carol, vendor]

