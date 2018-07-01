{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, NamedFieldPuns #-}
module Examples.ThreeBuyer where

import Control.Monad.State as State
import Control.Monad.Except as Except

import LocalType (LocalType, Location, Participant, Identifier, Transaction(..))
import qualified TypeContext 
import qualified LocalType
import qualified GlobalType

import Data.Map as Map (Map)
import Data.List as List
import qualified Data.Map as Map
import Data.Fix as Fix

import Utils ((|>), List)
import qualified Utils.Maybe as Maybe
import Zipper

import Session (Monitor(..), ExecutionState(..), Session, Error(..))
import qualified Session 
import Semantics (forward) 
import qualified Semantics
import Program (ProgramF(..), Value(..), Program)
import Queue (QueueError(..))
import qualified Queue

import qualified HighLevel as H
import Data.Void (Void)
import Debug.Trace as Debug
import qualified Interpreter

data MyParticipants = A | B | C | V 
    deriving (Show, Eq, Ord, Enum, Bounded)

data MyType = Title | Price | Share | Ok | Thunk | Address | Date
    deriving (Show, Eq, Ord)


globalType :: GlobalType.GlobalType MyParticipants MyType
globalType = do 
    GlobalType.transaction A V Title 
    GlobalType.transaction V A Price 
    GlobalType.transaction V B Price 
    GlobalType.transaction A B Share 
    GlobalType.transaction B A Ok 
    GlobalType.transaction B V Ok 
    GlobalType.transaction B C Share
    GlobalType.transaction B C Thunk
    GlobalType.transaction B V Address
    GlobalType.transaction V B Date
    GlobalType.end


alice :: H.HighLevelProgram a
alice = do 
    let h = VInt 42 
    H.send (VString "Logicomix" )
    p <- H.receive 
    H.send h
    ok <- H.receive 
    H.terminate
            

bob :: H.HighLevelProgram a
bob = do 
    thunk <- 
        H.function $ \_ -> do
            H.send (VString "Lucca, 55100")
            d <- H.receive
            H.terminate

    price <- H.receive 
    share <- H.receive 
    {-
    H.ifThenElse (price `H.lessThan` VInt 79)
        ( do
            H.send (VBool True)
            H.send (VBool True)
        )
        ( do
            H.send (VBool False)
            H.send (VBool False)
        )
    -}
    H.send (VBool True)
    H.send (VBool True)
    H.send share
    H.send thunk 
    H.terminate


carol :: H.HighLevelProgram a
carol = do 
    h <- H.receive 
    code <- H.receive 
    H.applyFunction code VUnit
    H.terminate


vendor :: H.HighLevelProgram a
vendor = do 
    let price title = VInt 42
        date = VString "2018-03-14"

    t <- H.receive 
    H.send (price t) 
    H.send (price t) 
    ok <- H.receive 
    a <- H.receive 
    H.send date
    H.terminate

executionState = 
    Interpreter.constructExecutionState globalType $
        zip (List.map show [minBound..(maxBound :: MyParticipants)]) [ alice, bob, carol, vendor]

steps = 
    ( do
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

            forward "l2" 
            forward "l2" 
            forward "l1" 
            forward "l2" 
            forward "l4" 

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
    )
            |> flip State.runStateT executionState

