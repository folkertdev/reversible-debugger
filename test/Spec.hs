{-# LANGUAGE PatternSynonyms #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Trans.State as State 

import Program (Program, ProgramF(..), Value(..), renameVariable, IntOperator(..), terminate)
import Session (Monitor(..), ExecutionState(..), Session, Error(..))
import Semantics (forward_, backward_)
import qualified Semantics
import qualified Queue
import Zipper (Zipper(..))
import LocalType (LocalType(..), TypeContext) 
import qualified LocalType 
import qualified GlobalType 

import qualified Data.Map as Map
import Data.List as List
import Data.Fix

import qualified Examples.RecursiveChoice as RecursiveChoice
import qualified Examples.NestedRecursion as NestedRecursion

import qualified HighLevel as H

type List = []

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
            state = executionState Queue.empty
                [("A", monitor1, compileAlice $ H.send VUnit)
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive
                    H.terminate
                 )
                ]

            newMonitor1 = createMonitor (LocalType.backwardSend "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (LocalType.backwardReceive "B" "A" "var0" "v0" "unit", LocalType.end) 
                    (Map.singleton "v0" VUnit)

            newState = executionState (Queue.enqueueHistory ("A", "B", VUnit) Queue.empty)
                [("A", newMonitor1, compileAlice  H.terminate)
                ,("B", newMonitor2, compileBob H.terminate)
                ]
        in do
            forwardN ["A", "B"] state `shouldBe` Right newState
            (backwardN [ "B", "A" ] =<< forwardN ["A", "B"] state) `shouldBe` Right state

    it "send/receive fails when sender is not synced" $ 
        let 
            monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = executionState Queue.empty
                [("A", monitor1, compileAlice $ do
                    H.send VUnit
                    v <- H.create VUnit 
                    H.terminate
                 )
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive
                    v <- H.create VUnit 
                    H.terminate
                 )
                ]

            cont = "(SendOrReceive (Transaction (TSend {owner = \"A\", receiver = \"B\", tipe = \"unit\", continuation = ()})) (Fix Hole))"
            actual = "Assignment {visibleName = \"var0\", internalName = \"v1\", continuation = Fix " ++ cont ++ "}"
            errorMessage = Session.SynchronizationError $ "the sender's previous instruction is not a Send, but " ++ actual

        in 
            (backwardN [ "B", "A" ] =<< forwardN ["A", "B", "A" ] state) `shouldBe` Left errorMessage 

    it "send/receive fails when receiver is not synced" $ 
        let 
            monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = executionState Queue.empty
                [("A", monitor1, compileAlice $ do
                    H.send VUnit
                    v <- H.create VUnit 
                    H.terminate
                 )
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive
                    v <- H.create VUnit 
                    H.terminate
                 )
                ]

            cont = "(SendOrReceive (Transaction (TSend {owner = \"A\", receiver = \"B\", tipe = \"unit\", continuation = ()})) (Fix Hole))"
            actual = "Assignment {visibleName = \"var0\", internalName = \"v1\", continuation = Fix " ++ cont ++ "}"
            errorMessage = Session.SynchronizationError $ "the sender's previous instruction is not a Send, but " ++ actual

        in 

            ( backwardN [ "B", "B", "A" ] =<<  forwardN ["A", "B", "B" ] state) `shouldBe` Right state


    it "IfThenElse behaves for then" $ 
        let localType = LocalType.end 

            monitor = createMonitor (id, localType) Map.empty
            
            verdict = True
            condition = VBool verdict

            thenBranch = H.send (VInt 42)
            elseBranch = H.send VUnit
            state = executionState Queue.empty
                [("A", monitor, compileAlice $ H.ifThenElse condition thenBranch elseBranch) 
                ]

            newMonitor = createMonitor (Fix . LocalType.Branched condition verdict (compileAlice elseBranch), localType) Map.empty
            newState = executionState Queue.empty
                [("A", newMonitor, compileAlice $ H.send (VInt 42))
                ]
        in do
            forwardN ["A"] state `shouldBe` Right newState
            (backwardN [ "A" ] =<< forwardN ["A"] state) `shouldBe` Right state

    it "choise behaves" $ 
        let 
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

        in do
            let Right base = forwardN [ "A", "A", "A", "B", "B", "B" ]  state
            (backwardN [ "A", "B" ] =<< forwardN [ "B", "A" ] base) `shouldBe` Right base

    it "choise fails when offerer is unsynced" $ 
        let 
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

            errorMessage = 
                "the offerer's previous instruction is not a Offer, but SendOrReceive (Atom V) ("
                    ++ "Fix (Offered {owner = \"A\", selector = \"B\", picked = Zipper ([],(\"recurse\","
                    ++ "Fix (Application \"v0\" (VIntOperator (VReference \"v1\") Add (VInt (-1)))),Fix (Atom V)),[(\"end\",Fix NoOp,Fix (Atom End))]), continuation = "
                    ++ "Fix (SendOrReceive (Transaction (TSend {owner = \"A\", receiver = \"B\", tipe = \"number\", continuation = ()})) (Fix (Application \"v1\" \"k0\" ("
                    ++ "Fix (Assignment {visibleName = \"var0\", internalName = \"v0\", continuation = Fix (SendOrReceive (Atom (R ())) (Fix Hole))})))))}))"
        in do
            let Right base = forwardN [ "A", "A", "A", "B", "B", "B" ]  state
            (backwardN [ "A", "B" ] =<< forwardN [ "B", "A", "A" ] base) `shouldBe` Left (SynchronizationError errorMessage)

    it "choise fails when selector is unsynced" $ 
        let 
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

            errorMessage = 
                "the selector's previous instruction is not a Select, but Application \"v5\" \"k2\" (Fix (SendOrReceive (Atom V) ("
                    ++ "Fix (Selected {owner = \"B\", offerer = \"A\", selection = Zipper ([],(\"recurse\",VComparison (VReference \"v4\") GT (VInt 0),"
                    ++ "Fix (Application \"v2\" VUnit),Fix (Atom V)),[(\"end\",VBool True,Fix NoOp,Fix (Atom End))]), continuation = "
                    ++ "Fix (SendOrReceive (Transaction (TReceive {owner = \"B\", sender = \"A\", names = Just (\"var2\",\"v4\"), tipe = \"number\", continuation = ()})) ("
                    ++ "Fix (Application \"v3\" \"k1\" (Fix (Assignment {visibleName = \"var0\", internalName = \"v2\", continuation = Fix (SendOrReceive (Atom (R ())) (Fix Hole))})))))}))))"
        in do
            let Right base = forwardN [ "A", "A", "A", "B", "B", "B" ]  state
            (backwardN [ "A", "B" ] =<< forwardN [ "B", "A", "B" ] base) `shouldBe` Left (SynchronizationError errorMessage)

    it "assignment behaves" $ 
        let 
            monitor = createMonitor (id, LocalType.end) Map.empty
            newMonitor = createMonitor (Fix . LocalType.Assignment "var0" "v0", LocalType.end) (Map.singleton "v0" VUnit) 

            state = executionState Queue.empty
                [("A", monitor, compileAlice $ do
                    x <- H.create VUnit 
                    H.terminate
                 )
                ]

            newState = executionState Queue.empty
                [("A", newMonitor, compileAlice H.terminate)
                ]
        in do
            forwardN ["A"] state `shouldBe` Right newState
            (backwardN [ "A" ] =<< forwardN [ "A" ] state) `shouldBe` Right state

    it "recursion behaves" $ 
        let 
            monitor1 = createMonitor (id, RecursiveChoice.localTypes Map.! "A") Map.empty
            monitor2 = createMonitor (id, RecursiveChoice.localTypes Map.! "B") Map.empty

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

        in do
            {- The forward action on A produces two history items: 
                    * an application history 
                    * a history for V, the recursion variable
                    in that order - which might be wrong. anyway, we have to thus move backward twice
            -} 
            let Right base = forwardN [ "A", "A", "A", "B", "B", "B", "B", "B", "A"] state
            (backwardN [ "A", "A" ] =<< forwardN [ "A" ] base) `shouldBe` Right base

    it "nested recursion behaves" $ 
        let 
            monitor1 = createMonitor (id, NestedRecursion.localTypes Map.! "A") Map.empty
            monitor2 = createMonitor (id, NestedRecursion.localTypes Map.! "B") Map.empty

            state = executionState Queue.empty
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

            state = executionState Queue.empty
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
            (backwardN [ "B", "A", "B", "A" ] =<< forwardN [ "A", "B" ] base) `shouldBe` Right base

testForward = describe "forward_" $ do 
    let forwarder = Semantics.forwardTestable "L1" "A" 

        forwarderN []     state = Right state 
        forwarderN (p:ps) state = 
            case Semantics.forwardTestable "L1" p state of 
                Left e -> Left e
                Right v -> forwarderN ps v

    it "terminate doesn't change state" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            state = executionState Queue.empty [("A", monitor, Program.terminate)]
        in
            forwarder state `shouldBe` Right state 

    it "literal doesn't change state" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            state = executionState Queue.empty [("A", monitor, Fix $ Program.Literal VUnit)]

            newState = executionState Queue.empty [("A", monitor, Program.terminate)]
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

            state = executionState Queue.empty [("A", monitor, compileAlice program)] 

            newState = executionState Queue.empty [("A", newMonitor, Fix $ Program.Application "v0" VUnit )]
        in
            forwarder state `shouldBe` Right newState 

    it "let assigns to correct participant" $ 
        let 

            localTypes = LocalType.projections globalType

            newMonitors = 
                createMonitor 
                (Fix . LocalType.Assignment "var0" "v0" , LocalType.end) 
                (Map.singleton "v0" (VFunction "var1" (Fix (Application "v0" VUnit))))

            
            globalType =
                GlobalType.transaction "B" "C" "thunk"
                $ GlobalType.transaction "B" "A" "address"
                $ GlobalType.transaction "A" "B" "amount"
                  GlobalType.end

            bob = do 
                thunk <- 
                    H.function $ \_ -> do
                        H.send (VString "Lucca, 55100")
                        d <- H.receive
                        H.terminate

                H.send thunk 

            carol = do 
                code <- H.receive 
                H.applyFunction code VUnit

            alice = do 
                address <- H.receive 
                H.send (VInt 42)

            state = 
                executionState Queue.empty 
                    [ ("A", createMonitor (id, localTypes Map.! "A") Map.empty, H.compile "Location1" "A" alice)
                    , ("B", createMonitor (id, localTypes Map.! "B") Map.empty, H.compile "Location1" "B" bob)
                    , ("C", createMonitor (id, localTypes Map.! "C") Map.empty, H.compile "Location1" "C" carol)
                    ]

            newQueue = 
                List.foldr Queue.enqueueHistory Queue.empty 
                    [("A","B",VInt 42)
                    ,("B","A",VString "Lucca, 55100")
                    ,("B","C",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})})))
                    ]

            aliceType = 
                (LocalType.backwardSend "A" "B" "amount" . LocalType.backwardReceive "A" "B" "var0" "v3" "address", LocalType.end)

            bobType = 
                ( LocalType.backwardReceive "B" "A" "var2" "v4" "amount" . LocalType.backwardSend "B" "A" "address" . LocalType.backwardSend "B" "C" "thunk" . Fix . LocalType.Assignment "var0" "v0", LocalType.end)
                

            carolType = 
                ( Fix . LocalType.Application "v2" "k0" . LocalType.backwardReceive "C" "B" "var0" "v3" "thunk", LocalType.end )

            bobStore = 
                Map.fromList [("v0",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})}))),("v4",VInt 42)]

            carolStore = 
                Map.fromList [("v1",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})})))]

            newState = 
                executionState newQueue 
                    [ ("A", createMonitor aliceType (Map.fromList [("v3",VString "Lucca, 55100")]), H.compile "Location1" "A" H.terminate)
                    , ("B", createMonitor bobType bobStore, H.compile "Location1" "B" H.terminate)
                    , ("C", createMonitor aliceType carolStore, H.compile "Location1" "C" H.terminate)
                    ]
        in
            forwardN ["B", "B", "C", "C", "C", "A", "A", "C" ]  state `shouldBe` Right newState 


    it "receive adds its variable to the correct owner" $ 
        let 
            globalType = 
                GlobalType.transaction "C" "B" "thunk" $ 
                    GlobalType.transaction "A" "C" "fourtyTwo" 
                    GlobalType.end

            localTypes = LocalType.projections globalType

            alice = H.send (VInt 42)
    
            bob = do
                thunk <- H.receive
                H.applyFunction thunk VUnit

            carol = do 
                thunk <- H.function $ \_ -> do
                    v <- H.receive
                    H.terminate

                H.send thunk
                
            state = 
                executionState Queue.empty 
                    [ ("A", createMonitor (id, localTypes Map.! "A") Map.empty, H.compile "Location1" "A" alice)
                    , ("B", createMonitor (id, localTypes Map.! "B") Map.empty, H.compile "Location1" "B" bob)
                    , ("C", createMonitor (id, localTypes Map.! "C") Map.empty, H.compile "Location1" "C" carol)
                    ]

            aType = 
                (LocalType.backwardSend "A" "C" "fourtyTwo", LocalType.end)

            bType = 
                (Fix . LocalType.Application "v2" "k0" . LocalType.backwardReceive "B" "C" "var0" "v1" "thunk", LocalType.end)

            cType = 
                (LocalType.backwardReceive "C" "A" "var2" "v3" "fourtyTwo" . LocalType.backwardSend "C" "B" "thunk" . Fix . LocalType.Assignment "var0" "v0", LocalType.end)

            thunk = VFunction "var1" (Fix (Receive {owner = "C", variableName = "var2", continuation = Fix NoOp}))

            bStore = Map.fromList [ ("v1", thunk), ("v2", VUnit) ]
            cStore = Map.fromList [ ("v0", thunk) , ("v3", VInt 42) ]

            newState = 
                executionState (Queue.enqueueHistory ("A", "C", VInt 42) $ Queue.enqueueHistory ("C", "B", thunk) Queue.empty)
                    [ ("A", createMonitor aType Map.empty, H.compile "Location1" "A" H.terminate)
                    , ("B", (createMonitor bType  bStore) { _applicationHistory = Map.fromList [("k0",("v1",VUnit))] } , H.compile "Location1" "B" H.terminate)
                    , ("C", createMonitor cType  cStore , H.compile "Location1" "C" H.terminate)
                    ]

        in forwardN [ "C", "C", "A", "B", "B", "B", "B" ] state `shouldBe` Right newState

    it "send behaves" $ 
        let monitor = createMonitor (id, LocalType.send "A" "A" "unit" LocalType.end) Map.empty
            state = executionState Queue.empty [("A", monitor, compileAlice $ H.send VUnit)] 

            newMonitor = createMonitor (LocalType.backwardSend "A" "A" "unit", LocalType.end) Map.empty
            newState = executionState 
                (Queue.enqueue ("A", "A", VUnit) Queue.empty) 
                [("A", newMonitor, Program.terminate)]
        in
            forwarder state `shouldBe` Right newState 



    let sendReceiveProgram monitor1 monitor2 = 
            executionState Queue.empty
                [("A", monitor1, compileAlice $ H.send VUnit) 
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive 
                    H.terminate
                 )
                ]

    it "send/receive behaves" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty

            state = sendReceiveProgram monitor1 monitor2

            newMonitor1 = createMonitor (LocalType.backwardSend "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (LocalType.backwardReceive "B" "A" "var0" "v0" "unit", LocalType.end) 
                    (Map.singleton "v0" VUnit)

            newState = executionState 
                (Queue.enqueueHistory ("A", "B", VUnit) Queue.empty) 
                [("A", newMonitor1, Program.terminate)
                ,("B", newMonitor2, Program.terminate)
                ]
        in
            forwarderN [ "A", "B" ] state `shouldBe` Right newState 

    it "receive errors when sender owner is incorrect" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "C" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = sendReceiveProgram monitor1 monitor2

            errorMessage = 
                QueueError "Receive" (Queue.InvalidBackwardQueueItem "receiver mismatch; the type expects B but the queue contains C")
        in
            forwarderN [ "A", "B" ] state `shouldBe` Left errorMessage 

    it "receive errors when sender owner is incorrect" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "C" "unit" LocalType.end) Map.empty
            state = sendReceiveProgram monitor1 monitor2

            errorMessage = 
                QueueError "Receive" (Queue.InvalidBackwardQueueItem "sender mismatch; the type expects C but the queue contains A")
        in
            forwarderN [ "A", "B" ] state `shouldBe` Left errorMessage 

    let offerSelectProgram monitor1 monitor2 = 
            executionState Queue.empty
                [("A", monitor1, compileAlice $ 
                    H.offer 
                        [ ("x", H.terminate)
                        , ("y", H.terminate)
                        ]
                 )
                ,("B", monitor2, compileBob $ 
                    H.select 
                        [ ("x", VBool True, H.terminate)
                        , ("y", VBool False, H.terminate)
                        ]
                 )
                ]

    it "offer/select behaves" $ 
        let globalType = 
                GlobalType.oneOf "A" "B" [ (,) "x" GlobalType.end , (,) "y" GlobalType.end ]

            type1 = LocalType.offer "A" "B" [ ("x",LocalType.end),("y",LocalType.end)]
            type2 = LocalType.select "B" "A" [("x",LocalType.end),("y",LocalType.end)]
                        
            monitor1 = createMonitor (id, type1) Map.empty
            monitor2 = createMonitor (id, type2) Map.empty

            state = offerSelectProgram monitor1 monitor2

            picked = Zipper ([],("x",Fix NoOp,LocalType.end),[("y",Fix NoOp, LocalType.end)])
            selection = Zipper ([],("x",VBool True,Fix NoOp,LocalType.end),[("y",VBool False,Fix NoOp,LocalType.end)])

            newMonitor1 = createMonitor (Fix . LocalType.Offered "A" "B" picked, LocalType.end)    Map.empty
            newMonitor2 = createMonitor (Fix . LocalType.Selected "B" "A" selection, LocalType.end) Map.empty

            newState = executionState 
                (Queue.enqueueHistory ("A", "B", VLabel "x") Queue.empty) 
                [("A", newMonitor1, Program.terminate)
                ,("B", newMonitor2, Program.terminate)
                ]
        in
            forwarderN [ "B", "A" ] state `shouldBe` Right newState 

    it "offer errors when offerer owner is incorrect" $ 
        let globalType = 
                GlobalType.oneOf "A" "B" [ (,) "x" GlobalType.end , (,) "y" GlobalType.end ]

            type1 = LocalType.offer "A" "C" [ ("x",LocalType.end),("y",LocalType.end)]
            type2 = LocalType.select "B" "A" [("x",LocalType.end),("y",LocalType.end)]
                        
            monitor1 = createMonitor (id, type1) Map.empty
            monitor2 = createMonitor (id, type2) Map.empty

            state = offerSelectProgram monitor1 monitor2

            errorMessage =
                QueueError "Offer" (Queue.InvalidBackwardQueueItem "receiver mismatch; the type expects C but the queue contains B")
        in
            forwarderN [ "B", "A" ] state `shouldBe` Left errorMessage

    it "offer errors when offerer owner is incorrect" $ 
        let globalType = 
                GlobalType.oneOf "A" "B" [ (,) "x" GlobalType.end , (,) "y" GlobalType.end ]

            type1 = LocalType.offer "A" "B" [ ("x",LocalType.end),("y",LocalType.end)]
            type2 = LocalType.select "B" "C" [("x",LocalType.end),("y",LocalType.end)]
                        
            monitor1 = createMonitor (id, type1) Map.empty
            monitor2 = createMonitor (id, type2) Map.empty

            state = offerSelectProgram monitor1 monitor2

            errorMessage =
                QueueError "Offer" (Queue.InvalidBackwardQueueItem "sender mismatch; the type expects A but the queue contains C")
        in
            forwarderN [ "B", "A" ] state `shouldBe` Left errorMessage

testRenameVariable = describe "renameVariable" $ do
    let renamer = unFix . renameVariable "x" "y" . Fix

    it "renames function name" $
          renamer (Program.Application "x" VUnit) `shouldBe` Program.Application "y" VUnit

    it "renames reference in function argument" $
        renamer (Program.Application "f" (VReference "x")) 
            `shouldBe` Program.Application "f" (VReference "y")



    it "renames in assignment right-hand side" $
        renamer (Program.Let "v" (VReference "x") Program.terminate)
            `shouldBe` Program.Let "v" (VReference "y") Program.terminate

    it "renames in send payload right-hand side" $
        renamer (Program.Send "owner" (VReference "x") Program.terminate)
            `shouldBe` Program.Send "owner" (VReference "y") Program.terminate
    {-
    it "renames in IfThenElse condition" $
        renamer (Semantics.ifThenElse (VReference "x") Program.terminate Program.terminate)
            `shouldBe` Semantics.ifThenElse (VReference "y") Program.terminate Program.terminate
    -}

    it "renames in literal" $
        renamer (Program.Literal (VReference "x")) 
            `shouldBe` Program.Literal (VReference "y") 

    it "renames in nested condition" $
        let lit = Fix . Program.Literal . VReference
            example varname = Program.IfThenElse (VReference "q") (lit varname) (lit varname)
        in
            renamer (example "x") `shouldBe` example "y"

createMonitor :: ( TypeContext (Program Value) Value String -> TypeContext (Program Value) Value  String
                 , LocalType String
                 ) 
              -> Map.Map String Value -> Monitor Value String
createMonitor (previous, localType) store = Monitor 
            { _localType = LocalType.Unsynchronized (previous $ Fix LocalType.Hole,  localType) 
            , _store = store 
            , _applicationHistory = Map.empty
            , _recursiveVariableNumber = 0
            , _recursionPoints = []
            }


executionState :: Queue.Queue Value -> List (String, Monitor Value String, Program Value) -> ExecutionState Value
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
                , queue = queue -- Queue.empty
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }
