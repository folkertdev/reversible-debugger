
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Trans.State as State 

import Semantics (List, Monitor(..), ExecutionState(..), Program, ProgramF(..), Value(..), renameVariable, Queue, emptyQueue , Session, forward_, emptyQueue, Error, IntOperator(..))
import qualified Semantics
import LocalType (LocalType, TypeContext) 
import qualified LocalType 
import qualified GlobalType 

import qualified Data.Map as Map
import Data.List as List
import Data.Fix

import qualified Examples.RecursiveChoice as RecursiveChoice
import qualified Examples.NestedRecursion as NestedRecursion

import qualified HighLevel as H

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

compileAlice = H.compile "Location1" "A"
compileBob = H.compile "Location1" "B"

testBackward = describe "backward" $ do 
    it "send/receive behaves" $ 
        let 
            monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = executionState emptyQueue
                [("A", monitor1, compileAlice $ H.send VUnit)
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive
                    H.terminate
                 )
                ]

            newMonitor1 = createMonitor (LocalType.backwardSend "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (LocalType.backwardReceive "B" "A" "unit" "var0" "v0", LocalType.end) 
                    (Map.singleton "v0" VUnit)

            newState = executionState (Semantics.enqueueHistory ("A", "B", VUnit) emptyQueue)
                [("A", newMonitor1, compileAlice  H.terminate)
                ,("B", newMonitor2, compileBob H.terminate)
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
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

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
            newMonitor = createMonitor (Fix . LocalType.Assignment "var0" "v0", LocalType.end) (Map.singleton "v0" VUnit) 

            state = executionState emptyQueue
                [("A", monitor, compileAlice $ do
                    x <- H.create VUnit 
                    H.terminate
                 )
                ]

            newState = executionState emptyQueue
                [("A", newMonitor, compileAlice H.terminate)
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

            alice = H.compile "Location1" "A" $ do
                H.send VUnit  
                H.offer [ ("continue", H.send VUnit), ("end", H.terminate) ]

            bob = H.compile "Location1" "B" $ do
                y <- H.receive
                H.select 
                    [ ("continue", VBool True, do
                        x <- H.receive
                        H.terminate
                      )
                    , ("end", VBool False, H.terminate) 
                    ]

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
            state = executionState emptyQueue [("A", monitor, Fix $ Semantics.Literal VUnit)]

            newState = executionState emptyQueue [("A", monitor, Semantics.terminate)]
        in
            forwarder state `shouldBe` Right newState 

    it "let renames in function bodies" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            newMonitor = 
                createMonitor 
                (Fix . LocalType.Assignment "var0" "v0" , LocalType.end) 
                (Map.singleton "v0" (VFunction "var1" (Fix (Application "v0" VUnit))))

            program = do
                x <- H.recursiveFunction $ \self _ -> H.applyFunction self VUnit 
                H.applyFunction x VUnit

            state = executionState emptyQueue [("A", monitor, compileAlice program)] 

            newState = executionState emptyQueue [("A", newMonitor, Fix $ Semantics.Application "v0" VUnit )]
        in
            forwarder state `shouldBe` Right newState 

    it "send behaves" $ 
        let monitor = createMonitor (id, LocalType.send "A" "A" "unit" LocalType.end) Map.empty
            state = executionState emptyQueue [("A", monitor, compileAlice $ H.send VUnit)] 

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
                [("A", monitor1, compileAlice $ H.send VUnit) 
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive 
                    H.terminate
                 )
                ]

            newMonitor1 = createMonitor (LocalType.backwardSend "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (LocalType.backwardReceive "B" "A" "unit" "var0" "v0", LocalType.end) 
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
    let renamer = unFix . renameVariable "x" "y" . Fix

    it "renames function name" $
          renamer (Semantics.Application "x" VUnit) `shouldBe` Semantics.Application "y" VUnit

    it "renames reference in function argument" $
        renamer (Semantics.Application "f" (VReference "x")) 
            `shouldBe` Semantics.Application "f" (VReference "y")



    it "renames in assignment right-hand side" $
        renamer (Semantics.Let "v" (VReference "x") Semantics.terminate)
            `shouldBe` Semantics.Let "v" (VReference "y") Semantics.terminate

    it "renames in send payload right-hand side" $
        renamer (Semantics.Send "owner" (VReference "x") Semantics.terminate)
            `shouldBe` Semantics.Send "owner" (VReference "y") Semantics.terminate
    {-
    it "renames in IfThenElse condition" $
        renamer (Semantics.ifThenElse (VReference "x") Semantics.terminate Semantics.terminate)
            `shouldBe` Semantics.ifThenElse (VReference "y") Semantics.terminate Semantics.terminate
    -}

    it "renames in literal" $
        renamer (Semantics.Literal (VReference "x")) 
            `shouldBe` Semantics.Literal (VReference "y") 

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
