
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
import qualified GlobalType 

import qualified Data.Map as Map
import Data.List as List
import Data.Fix

import qualified RecursiveChoice
import qualified NestedRecursion

main :: IO ()
main = hspec $ describe "all" Main.all
    

all = do 
    testForward
    testBackward

    testRenameVariable

specific  = 
    undefined


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

testBackward = describe "backward" $ do 
    it "send/receive behaves" $ 
        let 
            monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
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
    {- 
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
       -}

    it "choise behaves" $ 
        let 
            monitor1 = createMonitor (id, RecursiveChoice.localTypes Map.! "A") Map.empty
            monitor2 = createMonitor (id, RecursiveChoice.localTypes Map.! "B") Map.empty

            state = executionState emptyQueue
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

        in do
            let Right base = forwardN [ "A", "A", "A", "B", "B", "B" ] state
            (backwardN [ "A", "B" ] =<< forwardN ["B", "A" ] base) `shouldBe` Right base

    it "assignment behaves" $ 
        let 
            monitor = createMonitor (id, LocalType.end) Map.empty
            newMonitor = createMonitor (Fix . LocalType.Assignment "x" "v0", LocalType.end) (Map.singleton "v0" VUnit) 

            state = executionState emptyQueue
                [("A", monitor, Semantics.letBinding "x" VUnit Semantics.terminate)
                ]

            newState = executionState emptyQueue
                [("A", newMonitor, Semantics.terminate)
                ]
        in do
            forwardN ["A"] state `shouldBe` Right newState
            (backwardN [ "A" ] =<< forwardN [ "A" ] state) `shouldBe` Right state

    it "recursion behaves" $ 
        let 
            monitor1 = createMonitor (id, RecursiveChoice.localTypes Map.! "A") Map.empty
            monitor2 = createMonitor (id, RecursiveChoice.localTypes Map.! "B") Map.empty

            state = executionState emptyQueue
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

        in do
            {- The forward action on A produces two history items: 
                    * an application history 
                    * a history for V, the recursion variable
                    in that order - which might be wrong. anyway, we have to thus move backward twice
            -} 
            let Right base = forwardN [ "A", "A", "A", "B", "B", "B", "B", "A"] state
            (backwardN [ "A", "A" ] =<< forwardN [ "A" ] base) `shouldBe` Right base

    it "nested recursion behaves" $ 
        let 
            monitor1 = createMonitor (id, NestedRecursion.localTypes Map.! "A") Map.empty
            monitor2 = createMonitor (id, NestedRecursion.localTypes Map.! "B") Map.empty

            state = executionState emptyQueue
                [ ("A", monitor1, NestedRecursion.alice)
                , ("B", monitor2, NestedRecursion.bob) 
                ]

        in do
            {- The forward action on A produces two history items: 
                    * an application history 
                    * a history for V, the recursion variable
                    in that order - which might be wrong. anyway, we have to thus move backward twice
            -} 
            let init = ["A", "B" ]  
                unwrap = [ "A", "A", "B", "B" ] 
                nested = unwrap ++ [  "B", "A" ]  
                Right base = forwardN (concat [ init, unwrap, nested, nested, nested, nested ]) state
            (backwardN [ "A", "A", "A", "A" ] =<< forwardN [ "A"  ] base) `shouldBe` Right base
            -- (backwardN [ "A", "A" ] =<< forwardN [ "A", "A"  ] base) `shouldBe` Right base
            -- print base

    it "nested recursion behaves 2" $ 
        let 
            globalType = 
                GlobalType.recurse $
                    GlobalType.recurse $
                        GlobalType.transaction "A" "B" "number" $
                            GlobalType.oneOf "A" "B" 
                                [ (,) "continue"  GlobalType.recursionVariable 
                                , (,) "end" GlobalType.end
                                ]
                        
            localTypes = LocalType.projections globalType
            monitorA = createMonitor (id, localTypes Map.! "A") Map.empty
            monitorB = createMonitor (id, localTypes Map.! "B") Map.empty

            alice = 
                Semantics.send "A" VUnit $ 
                    Semantics.offer "A" [ ("continue", Semantics.send "A" VUnit Semantics.terminate), ("end", Semantics.terminate) ]
            bob = 
                Semantics.receive "B" "y" $ 
                    Semantics.select "B" [ ("continue", VBool True, Semantics.receive "B" "x" Semantics.terminate), ("end", VBool False, Semantics.terminate) ]

            state = executionState emptyQueue
                [ ("A", monitorA, alice)
                , ("B", monitorB, bob) 
                ]

        in do
            {- The forward action on A produces two history items: 
                    * an application history 
                    * a history for V, the recursion variable
                    in that order - which might be wrong. anyway, we have to thus move backward twice
            -} 
            let Right base = forwardN [ "A", "B", "B", "A"] state
            -- (backwardN [ "A", "A" ] =<< forwardN [ "A" ] base) `shouldBe` Right base
            (backwardN [ "A", "A" ] =<< forwardN [ "A", "A" ] base) `shouldBe` Right base

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

    it "let renames in function bodies" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            newMonitor = createMonitor (Fix . LocalType.Assignment "x" "v0" , LocalType.end) (Map.singleton "v0" (VFunction "_" (Fix (Application "v0" VUnit))))

            program = letBinding "x" (VFunction "_" $ applyFunction "x" VUnit) $ applyFunction "x" VUnit 

            state = executionState emptyQueue [("A", monitor, program)] 

            newState = executionState emptyQueue [("A", newMonitor, applyFunction "v0" VUnit )]
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

    {-
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
    -}
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
    {-
    it "renames in IfThenElse condition" $
        renamer (Semantics.ifThenElse (VReference "x") Semantics.terminate Semantics.terminate)
            `shouldBe` Semantics.ifThenElse (VReference "y") Semantics.terminate Semantics.terminate
    -}

    it "renames in literal" $
        renamer (Semantics.literal (VReference "x")) 
            `shouldBe` Semantics.literal (VReference "y") 

    {-
    it "renames in nested condition" $
        let lit = Semantics.literal . VReference
        in
        renamer (Semantics.ifThenElse (VReference "q") (lit "x") (lit "x"))
            `shouldBe` Semantics.ifThenElse (VReference "q") (lit "y") (lit "y") 
    -}

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
