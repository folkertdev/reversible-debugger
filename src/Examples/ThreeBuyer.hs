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
    GlobalType.transaction V B Price 
    GlobalType.transaction V A Price 
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
    let price _ = VInt 42
        date = VString "2018-03-14"

    t <- H.receive 
    H.send (price t) 
    H.send (price t) 
    ok <- H.receive 
    a <- H.receive 
    H.send date
    H.terminate

initialProgram = 
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
            |> flip State.runStateT initialProgram

{-
participants: fromList [("A",Monitor {_localType = Unsynchronized (Fix (Transaction (TSend {owner = "A", receiver = "B", tipe = "Share", continuation = Fix (Transaction (TReceive {owner = "A", sender = "V", tipe = "Price", continuation = Fix (Transaction (TSend {owner = "A", receiver = "V", tipe = "Title", continuation = Fix IHole}))}))})),Fix (Transaction (TReceive {owner = "A", sender = "B", tipe = "Ok", continuation = Fix End}))), _recursiveVariableNumber = 0, _recursionPoints = [], _store = fromList [("v2",VInt 42)], _usedVariables = [Binding {_visibleName = "var0", _internalName = "v2"}], _applicationHistory = fromList []}),

("B",Monitor {_localType = Unsynchronized (Fix (IAssignment {owner = "B", continuation = Fix IHole}),Fix (Transaction (TReceive {owner = "B", sender = "V", tipe = "Price", continuation = Fix (Transaction (TReceive {owner = "B", sender = "A", tipe = "Share", continuation = Fix (Transaction (TSend {owner = "B", receiver = "A", tipe = "Ok", continuation = Fix (Transaction (TSend {owner = "B", receiver = "V", tipe = "Ok", continuation = Fix (Transaction (TSend {owner = "B", receiver = "C", tipe = "Share", continuation = Fix (Transaction (TSend {owner = "B", receiver = "C", tipe = "Thunk", continuation = Fix (Transaction (TSend {owner = "B", receiver = "V", tipe = "Address", continuation = Fix (Transaction (TReceive {owner = "B", sender = "V", tipe = "Date", continuation = Fix End}))}))}))}))}))}))}))}))), _recursiveVariableNumber = 0, _recursionPoints = [], _store = fromList [("v0",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})})))], _usedVariables = [Binding {_visibleName = "var0", _internalName = "v0"}], _applicationHistory = fromList []}),("C",Monitor {_localType = Unsynchronized (Fix IHole,Fix (Transaction (TReceive {owner = "C", sender = "B", tipe = "Share", continuation = Fix (Transaction (TReceive {owner = "C", sender = "B", tipe = "Thunk", continuation = Fix End}))}))), _recursiveVariableNumber = 0, _recursionPoints = [], _store = fromList [], _usedVariables = [], _applicationHistory = fromList []}),("V",Monitor {_localType = Unsynchronized (Fix (Transaction (TSend {owner = "V", receiver = "B", tipe = "Price", continuation = Fix (Transaction (TSend {owner = "V", receiver = "A", tipe = "Price", continuation = Fix (Transaction (TReceive {owner = "V", sender = "A", tipe = "Title", continuation = Fix IHole}))}))})),Fix (Transaction (TReceive {owner = "V", sender = "B", tipe = "Ok", continuation = Fix (Transaction (TReceive {owner = "V", sender = "B", tipe = "Address", continuation = Fix (Transaction (TSend {owner = "V", receiver = "B", tipe = "Date", continuation = Fix End}))}))}))), _recursiveVariableNumber = 0, _recursionPoints = [], _store = fromList [("v1",VString "Logicomix")], _usedVariables = [Binding {_visibleName = "var0", _internalName = "v1"}], _applicationHistory = fromList []})]
    -}
