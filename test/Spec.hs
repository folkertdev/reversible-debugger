
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad


import Types
import Text.ParserCombinators.Parsec as Parsec

import Control.Monad.Trans.State as State 

import Semantics (Monitor(..), ExecutionState(..), Program, ProgramF(..), Value(..), renameVariable, Queue, emptyQueue , Session, forward_, emptyQueue, letBinding, send, run, applyFunction, decrement, Error, IntOperator(..))
import qualified Semantics
import LocalType (LocalType, TypeContext) 
import qualified LocalType 

import qualified Data.Map as Map
import Data.List as List
import Data.Fix

main :: IO ()
main = hspec $ describe "all" $ do 
    testForward
    testBackward

    testRenameVariable
    
testBackward = describe "backward" $ do 
    let
        forwardN []     state = Right state 
        forwardN (p:ps) state = 
            case Semantics.forwardTestable "L1" p state of 
                Left e -> Left e
                Right v -> forwardN ps v

        backwardN []     state = Right state 
        backwardN (p:ps) state = 
            case Semantics.backwardTestable "L1" p state of 
                Left e -> Left e
                Right v -> backwardN ps v

    it "send/receive behaves" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = executionState emptyQueue
                [("A", monitor1, Semantics.send "A" VUnit Semantics.terminate)
                ,("B", monitor2, Semantics.receive "B" "x" Semantics.terminate)
                ]

            newMonitor1 = createMonitor (LocalType.backwardSend "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (LocalType.backwardReceive "B" "A" "unit" "x" "v0", LocalType.end) 
                    (Map.singleton "v0" VUnit)

            newState = executionState (Semantics.enqueueHistory ("A", "B", VUnit) emptyQueue)
                [("A", newMonitor1, Semantics.terminate)
                ,("B", newMonitor2, Semantics.terminate)
                ]
        in do
            forwardN ["A", "B"] state `shouldBe` Right newState
            (backwardN [ "B", "A" ] =<< forwardN ["A", "B"] state) `shouldBe` Right state

    it "IfThenElse behaves for then" $ 
        let thenType = LocalType.send "A" "B" "unit" LocalType.end
            elseType = LocalType.send "A" "C" "unit" LocalType.end
            localType = Fix $ LocalType.Select thenType elseType

            sender = LocalType.backwardSend "A" "B" "unit" 
            monitor = createMonitor (sender, localType) Map.empty
            
            verdict = True
            condition = VBool verdict
            state = executionState emptyQueue
                [("A", monitor, Semantics.ifThenElse condition Semantics.terminate Semantics.terminate) 
                ]

            history = 
                Fix . LocalType.Selected condition verdict (Semantics.terminate, elseType) . sender

            newMonitor = createMonitor (history, thenType) Map.empty
            newState = executionState emptyQueue
                [("A", newMonitor, Semantics.terminate)
                ]
        in do
            forwardN ["A"] state `shouldBe` Right newState
            (backwardN [ "A" ] =<< forwardN ["A"] state) `shouldBe` Right state



testForward = describe "forward_" $ do 
    let forwarder = Semantics.forwardTestable "L1" "A" 

        forwarderN []     state = Right state 
        forwarderN (p:ps) state = 
            case Semantics.forwardTestable "L1" p state of 
                Left e -> Left e
                Right v -> forwarderN ps v

    it "terminate doesn't change state" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            state = executionState emptyQueue [("A", monitor, Semantics.terminate)]
        in
            forwarder state `shouldBe` Right state 

    it "literal doesn't change state" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            state = executionState emptyQueue [("A", monitor, Semantics.literal VUnit)]

            newState = executionState emptyQueue [("A", monitor, Semantics.terminate)]
        in
            forwarder state `shouldBe` Right newState 

    it "send behaves" $ 
        let monitor = createMonitor (id, LocalType.send "A" "A" "unit" LocalType.end) Map.empty
            state = executionState emptyQueue [("A", monitor, Semantics.send "A" VUnit Semantics.terminate)]

            newMonitor = createMonitor (LocalType.backwardSend "A" "A" "unit", LocalType.end) Map.empty
            newState = executionState 
                (Semantics.enqueue ("A", "A", VUnit) emptyQueue) 
                [("A", newMonitor, Semantics.terminate)]
        in
            forwarder state `shouldBe` Right newState 

    it "send/receive behaves" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = executionState emptyQueue
                [("A", monitor1, Semantics.send "A" VUnit Semantics.terminate)
                ,("B", monitor2, Semantics.receive "B" "x" Semantics.terminate)
                ]

            newMonitor1 = createMonitor (LocalType.backwardSend "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (LocalType.backwardReceive "B" "A" "unit" "x" "v0", LocalType.end) 
                    (Map.singleton "v0" VUnit)

            newState = executionState 
                (Semantics.enqueueHistory ("A", "B", VUnit) emptyQueue) 
                [("A", newMonitor1, Semantics.terminate)
                ,("B", newMonitor2, Semantics.terminate)
                ]
        in
            forwarderN [ "A", "B" ] state `shouldBe` Right newState 

    it "IfThenElse behaves for then" $ 
        let thenType = LocalType.send "A" "B" "unit" LocalType.end
            elseType = LocalType.send "A" "C" "unit" LocalType.end
            localType = Fix $ LocalType.Select thenType elseType

            monitor = createMonitor (id, localType) Map.empty
            
            verdict = True
            condition = VBool verdict
            state = executionState emptyQueue
                [("A", monitor, Semantics.ifThenElse condition Semantics.terminate Semantics.terminate) 
                ]

            history = 
                Fix . LocalType.Selected condition verdict (Semantics.terminate, elseType) 

            newMonitor = createMonitor (history, thenType) Map.empty
            newState = executionState emptyQueue
                [("A", newMonitor, Semantics.terminate)
                ]
        in
            forwarderN [ "A" ] state `shouldBe` Right newState 

testRenameVariable = describe "renameVariable" $ do
    let renamer = renameVariable "x" "y" 

    it "renames function name" $
          renamer (Semantics.applyFunction "x" VUnit) `shouldBe` Semantics.applyFunction "y" VUnit

    it "renames reference in function argument" $
        renamer (Semantics.applyFunction "f" (VReference "x")) 
            `shouldBe` Semantics.applyFunction "f" (VReference "y")

    it "renames in assignment right-hand side" $
        renamer (Semantics.letBinding "v" (VReference "x") Semantics.terminate)
            `shouldBe` Semantics.letBinding "v" (VReference "y") Semantics.terminate

    it "renames in send payload right-hand side" $
        renamer (Semantics.send "owner" (VReference "x") Semantics.terminate)
            `shouldBe` Semantics.send "owner" (VReference "y") Semantics.terminate

    it "renames in IfThenElse condition" $
        renamer (Semantics.ifThenElse (VReference "x") Semantics.terminate Semantics.terminate)
            `shouldBe` Semantics.ifThenElse (VReference "y") Semantics.terminate Semantics.terminate

    it "renames in literal" $
        renamer (Semantics.literal (VReference "x")) 
            `shouldBe` Semantics.literal (VReference "y") 

    it "renames in nested condition" $
        let lit = Semantics.literal . VReference
        in
        renamer (Semantics.ifThenElse (VReference "q") (lit "x") (lit "x"))
            `shouldBe` Semantics.ifThenElse (VReference "q") (lit "y") (lit "y") 

createMonitor :: ( TypeContext (Program Value) Value String -> TypeContext (Program Value) Value  String
                 , LocalType String
                 ) 
              -> Map.Map String Value -> Monitor Value String
createMonitor (previous, localType) store = Monitor 
            { _localType = (previous $ Fix LocalType.Hole,  localType) 
            , _freeVariables = [] 
            , _store = store 
            , _applicationHistory = Map.empty
            , _reversible = False 
            , _recursiveVariableNumber = 0
            , _recursionPoints = []
            }


executionState :: Queue Value -> List (String, Monitor Value String, Program Value) -> ExecutionState Value
executionState queue values = 
    let
        participants = Map.fromList $ List.map (\(name, monitor, program) -> (name, monitor)) values

        locations = Map.singleton "L1" 
            $ Map.fromList 
            $ List.map (\(name, monitor, program) -> (name, program)) values
    in
        ExecutionState
            { variableCount = 0
            , locationCount = 1 
            , applicationCount = 0
            , participants = participants 
            , locations =  locations
                , queue = queue -- emptyQueue
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }
