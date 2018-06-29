{-# LANGUAGE PatternSynonyms #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Trans.State as State 

import Program (Program, ProgramF(..), Value(..), renameVariable, IntOperator(..), terminate)
import qualified Session
import Session (Monitor(..), ExecutionState(..), Session, Error(..), OtherOptions(..))
import Semantics (forward, backward)
import qualified Semantics
import qualified Queue
import Zipper (Zipper(..))
import LocalType (LocalType(..))
import qualified TypeContext
import qualified LocalType 
import qualified GlobalType 
import Utils ((|>), List)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Fix

import qualified Examples.RecursiveChoice as RecursiveChoice
import qualified Examples.NestedRecursion as NestedRecursion

import qualified HighLevel as H
import Data.Void (Void)

main :: IO ()
main = hspec $ describe "all" Main.all
    
data Participants = A | B | C | D | V  
    deriving (Show, Eq, Ord)

all = do 
    testForward
    testBackward

    testRenameVariable
    testProjection



forwardN :: List Int -> ExecutionState Value -> Either Error (ExecutionState Value)
forwardN []     state = Right state 
forwardN (p:ps) state = 
    case Semantics.forwardTestable ("l" ++ show p) state of 
        Left e -> Left e
        Right v -> forwardN ps v

backwardN :: List Int -> ExecutionState Value -> Either Error (ExecutionState Value)
backwardN []     state = Right state 
backwardN (p:ps) state = 
    case Semantics.backwardTestable ("l" ++ show p) state of 
        Left e -> Left e
        Right v -> backwardN ps v

compileAlice = id 
compileBob = id 

testProjection = describe "projection" $ 
    it "projection of choice" $ 
        let globalType = GlobalType.globalType $ 
                GlobalType.oneOf B A 
                    [ (,) "x" $ do 
                        GlobalType.transaction C B "sometype"
                        GlobalType.end 
                    , (,) "y" GlobalType.end 
                    ]

            given = LocalType.projections globalType
            expected = LocalType.end

            aType = 
                LocalType.offer "A" "B" 
                    [ ( "x"
                      , LocalType.select "A" "C" [ ("x", LocalType.end ) ] 
                      )
                    , ( "y"
                      , LocalType.select "A" "C" [ ("y", LocalType.end ) ] 
                      )
                    ]

            bType = LocalType.select "B" "A" [("x", LocalType.receive "B" "C" "sometype" LocalType.end),("y", LocalType.end)]

            cType = LocalType.offer "C" "A" [("x", LocalType.send "C" "B" "sometype" LocalType.end),("y", LocalType.end)]

        in do
            (given Map.! show A) `shouldBe` aType
            (given Map.! show B) `shouldBe` bType
            (given Map.! show C) `shouldBe` cType

testBackward = describe "backward" $ do 
    it "send/receive behaves" $ 
        let 
            monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = executionState Queue.empty
                [("A", monitor1, do 
                    H.send VUnit 
                    H.terminate
                 )
                ,("B", monitor2, do 
                    x <- H.receive
                    H.terminate
                 )
                ]

            newMonitor1 = createMonitor (TypeContext.Sent "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (TypeContext.Received "B" "A" "unit", LocalType.end) 
                    (Map.singleton "v0" VUnit)
                    |> Session.markVariableAsUsed "var0" "v0"

            newState = executionState (Queue.enqueueHistory ("A", "B", VUnit) Queue.empty)
                [("A", newMonitor1, compileAlice  H.terminate)
                ,("B", newMonitor2, compileBob H.terminate)
                ]
        in do
            forwardN [ 1, 2 ] state `shouldBe` Right newState
            (backwardN [ 2, 1 ] =<< forwardN [ 1, 2] state) `shouldBe` Right state

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

            message = "the sender's previous instruction is not a Send, but "
            actual = "Fix (IAssignment {owner = \"A\", continuation = Fix (Transaction (TSend {owner = \"A\", receiver = \"B\", tipe = \"unit\", continuation = Fix IHole}))})"
            errorMessage = Session.SynchronizationError $ message ++ actual

        in 
            (backwardN [ 2,1 ] =<< forwardN [ 1, 2, 1 ] state) `shouldBe` Left errorMessage 

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

            cont = "(LocalType (Transaction (TSend {owner = \"A\", receiver = \"B\", tipe = \"unit\", continuation = ()})) (Fix Hole))"
            actual = "Assignment {visibleName = \"var0\", internalName = \"v1\", continuation = Fix " ++ cont ++ "}"
            errorMessage = Session.SynchronizationError $ "the sender's previous instruction is not a Send, but " ++ actual

        in 

            ( backwardN [ 2,2,1 ] =<<  forwardN [ 1, 2, 2 ] state) `shouldBe` Right state


    it "IfThenElse behaves for then" $ 
        let localType = LocalType.end 

            monitor = createMonitor (id, localType) Map.empty
            
            verdict = True
            condition = VBool verdict

            thenBranch = do
                H.send (VInt 42)
                H.terminate

            elseBranch = do 
                H.send VUnit
                H.terminate

            state = executionState Queue.empty
                [("A", monitor, compileAlice $ H.ifThenElse condition thenBranch elseBranch) 
                ]

            newMonitor = createMonitor (TypeContext.Branched "A", localType) Map.empty
            newState = executionStateWithStack Queue.empty
                [("A", newMonitor, [ OtherBranch condition verdict (H.compile "A" elseBranch) ], compileAlice $ H.send (VInt 42) >> H.terminate)
                ]
        in do
            forwardN [ 1] state `shouldBe` Right newState
            (backwardN [ 1 ] =<< forwardN [1] state) `shouldBe` Right state

    it "application reverses correctly" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            program = do 
                function <- H.function $ \x -> H.terminate
                H.applyFunction function VUnit

            state = executionState Queue.empty
                [ ("A", monitor, program) 
                ]

            newMonitor = (createMonitor 
                (TypeContext.Application "A" "k0" . TypeContext.Assigned "A", LocalType.end) $ 
                Map.fromList [ ("v0",VFunction "var1" (Fix NoOp)),("v1",VUnit) ]
                 ) { _applicationHistory = Map.fromList [("k0",(VReference "v0",VUnit))] }  
                    |> Session.markVariableAsUsed "var0" "v0"
                    |> Session.markVariableAsUsed "v1" "v1"

            newState = executionState Queue.empty
                [ ("A", newMonitor, H.terminate) 
                ]

        in do
            forwardN [ 1,1,1 ] state `shouldBe` Right newState
            (backwardN [ 1,1,1 ] =<< forwardN [ 1,1,1 ] state) `shouldBe` Right state
            

    it "choice behaves" $ 
        let 
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

        in do
            let Right base = forwardN [ 1,1,1, 2,2,2]  state
            (backwardN [ 1,2 ] =<< forwardN [ 2, 1 ] base) `shouldBe` Right base

    it "choice fails when offerer is unsynced" $ 
        let 
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

            message = 
                "the offerer's previous instruction is not a Offer, but "
            expected = "Fix (IV (Fix (IOffered {owner = \"A\", selector = \"B\", picked = Zipper ([],(\"recurse\",Fix (Recursion V)),[(\"end\",Fix End)]), continuation = Fix (Transaction (TSend {owner = \"A\", receiver = \"B\", tipe = \"number\", continuation = Fix (IApplication \"A\" \"k0\" (Fix (IAssignment {owner = \"A\", continuation = Fix (IR (Fix IHole))})))}))})))"
        in do
            let Right base = forwardN [ 1,1,1, 2,2,2]  state
            (backwardN [ 1,2 ] =<< forwardN [ 2, 1,1 ] base) `shouldBe` Left (SynchronizationError $ message ++ expected )

    it "choice fails when selector is unsynced" $ 
        let 
            monitor name = createMonitor (id, RecursiveChoice.localTypes Map.! name) Map.empty
            monitor1 = monitor "A" 
            monitor2 = monitor "B" 

            state = executionState Queue.empty
                [ ("A", monitor1, RecursiveChoice.alice)
                , ("B", monitor2, RecursiveChoice.bob) 
                ]

            message = "the selector's previous instruction is not a Select, but " 
            expected = "Fix (IApplication \"B\" \"k2\" (Fix (IV (Fix (ISelected {owner = \"B\", offerer = \"A\", selection = Zipper ([],(\"recurse\",Fix (Recursion V)),[(\"end\",Fix End)]), continuation = Fix (Transaction (TReceive {owner = \"B\", sender = \"A\", tipe = \"number\", continuation = Fix (IApplication \"B\" \"k1\" (Fix (IAssignment {owner = \"B\", continuation = Fix (IR (Fix IHole))})))}))})))))"
        in do
            let Right base = forwardN [ 1,1,1, 2,2,2]  state
            (backwardN [ 1,2 ] =<< forwardN [2,1,2] base) `shouldBe` Left (SynchronizationError $ message ++ expected)

    it "assignment behaves" $ 
        let 
            monitor = createMonitor (id, LocalType.end) Map.empty
            newMonitor = createMonitor (TypeContext.Assigned "A", LocalType.end) (Map.singleton "v0" VUnit) 
                |> Session.markVariableAsUsed "var0" "v0"

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
            forwardN [ 1] state `shouldBe` Right newState
            (backwardN [ 1 ] =<< forwardN [ 1 ] state) `shouldBe` Right state

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
            let Right base = forwardN [ 1,1,1,  2,2,2,2,2, 1] state
            (backwardN [ 1,1 ] =<< forwardN [ 1 ] base) `shouldBe` Right base

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
            let init = [ 1,2 ]  
                unwrap = [ 1,1,2,2] 
                nested = unwrap ++ [ 2,1 ]  
                Right base = forwardN (concat [ init, unwrap, nested, nested, nested, nested ]) state
            (backwardN [ 1,1,1,1 ] =<< forwardN [ 1 ] base) `shouldBe` Right base
            -- (backwardN [ "A", "A" ] =<< forwardN [ "A", "A"  ] base) `shouldBe` Right base
            -- print base

    it "nested recursion behaves 2" $ 
        let 
            globalType = GlobalType.globalType $ 
                GlobalType.recurse $
                    GlobalType.recurse $ do
                        GlobalType.transaction A B "number"
                        GlobalType.oneOf B A
                            [ (,) "continue"  GlobalType.recursionVariable 
                            , (,) "end" GlobalType.end
                            ]
                        
            localTypes = LocalType.projections globalType
            monitorA = createMonitor (id, localTypes Map.! "A") Map.empty
            monitorB = createMonitor (id, localTypes Map.! "B") Map.empty

            alice = do
                H.send VUnit  
                H.offer [ ("continue", H.send VUnit >> H.terminate), ("end", H.terminate) ]

            bob = do 
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
            -- print $ forwardN [ 1, 2, 2, 1, 1, 2 ] state
            let Right base = forwardN [ 1, 2 ] state
            -- (backwardN [ "A", "A" ] =<< forwardN [ "A" ] base) `shouldBe` Right base
            (backwardN [ 2, 1, 1, 2, 1, 2 ] =<< forwardN [ 2, 1, 1, 2 ] base) `shouldBe` Right base
            -- 2,1,2,1 and 1,2
            {-
            -}
            return ()

testForward = describe "forward_" $ do 
    let forwarder = Semantics.forwardTestable "l1" 

    it "terminate doesn't change state" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            state = executionState Queue.empty [("A", monitor, H.terminate)]
        in
            forwarder state `shouldBe` Right state 

    it "let renames in function bodies" $ 
        let monitor = createMonitor (id, LocalType.end) Map.empty
            newMonitor = 
                createMonitor 
                (TypeContext.Assigned "A", LocalType.end) 
                (Map.singleton "v0" (VFunction "var1" (Fix (Application "A" (VReference "v0") VUnit))))
                |> Session.markVariableAsUsed "var0" "v0"

            program = do
                x <- H.recursiveFunction $ \self _ -> H.applyFunction self VUnit 
                H.applyFunction x VUnit

            state = executionState Queue.empty [("A", monitor, compileAlice program)] 

            newState = executionState Queue.empty [("A", newMonitor, H.applyFunction (VReference "v0") VUnit)] -- Fix $ Program.Application "A" "v0" VUnit )]
        in
            forwarder state `shouldBe` Right newState 

    it "let assigns to correct participant" $ 
        let 

            localTypes = LocalType.projections globalType

            newMonitors = 
                createMonitor 
                (TypeContext.Assigned "A", LocalType.end) 
                (Map.singleton "v0" (VFunction "var1" (Fix (Application "A" (VReference "v0") VUnit))))

            
            globalType = GlobalType.globalType $ do
                GlobalType.transaction B C "thunk"
                GlobalType.transaction B A "address"
                GlobalType.transaction A B "amount"
                GlobalType.end

            bob = do 
                thunk <- 
                    H.function $ \_ -> do
                        H.send (VString "Lucca, 55100")
                        d <- H.receive
                        H.terminate

                H.send thunk 
                H.terminate

            carol = do 
                code <- H.receive 
                H.applyFunction code VUnit
                H.terminate

            alice = do 
                address <- H.receive 
                H.send (VInt 42)
                H.terminate

            state = 
                executionState Queue.empty 
                    [ ("A", createMonitor (id, localTypes Map.! "A") Map.empty, alice)
                    , ("B", createMonitor (id, localTypes Map.! "B") Map.empty, bob)
                    , ("C", createMonitor (id, localTypes Map.! "C") Map.empty, carol)
                    ]

            newQueue = 
                List.foldr Queue.enqueueHistory Queue.empty 
                    [("A","B",VInt 42)
                    ,("B","A",VString "Lucca, 55100")
                    ,("B","C",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})})))
                    ]

            bobStore = 
                Map.fromList [("v0",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})}))),("v4",VInt 42)]

            carolStore = 
                Map.fromList [("v1",VFunction "var1" (Fix (Send {owner = "B", value = VString "Lucca, 55100", continuation = Fix (Receive {owner = "B", variableName = "var2", continuation = Fix NoOp})}))), ("v2",VUnit)]

            aliceMonitor = 
                let tipe = 
                        ( TypeContext.Sent "A" "B" "amount" . TypeContext.Received "A" "B" "address"
                        , LocalType.end
                        )
                in
                createMonitor tipe (Map.fromList [("v3",VString "Lucca, 55100")])
                    |> Session.markVariableAsUsed "var0" "v3"

            bobMonitor = 
                let tipe = 
                        ( TypeContext.Received "B" "A" "amount" 
                        . TypeContext.Sent "B" "A" "address" 
                        . TypeContext.Sent "B" "C" "thunk" 
                        . TypeContext.Assigned "B"
                        , LocalType.end
                        )
                in
                createMonitor tipe bobStore
                    |> Session.markVariableAsUsed "var0" "v0"
                    |> Session.markVariableAsUsed "var2" "v4"

            carolMonitor = 
                let tipe = 
                        ( TypeContext.Application "C" "k0" . TypeContext.Received "C" "B" "thunk"
                        , LocalType.end 
                        )
                in
                createMonitor tipe carolStore
                    |> Session.markVariableAsUsed "var0" "v1"
                    |> Session.addApplication ("k0",(VReference "v1",VUnit))
                    |> Session.markVariableAsUsed "v2" "v2"

            newState = 
                executionState newQueue 
                    [ ("A", aliceMonitor, H.terminate) 
                    , ("B", bobMonitor, H.terminate)
                    , ("C", carolMonitor, H.terminate)
                    ]
        in
            forwardN [2,2, 3,3,3, 1,1, 3 ]  state `shouldBe` Right newState 

    it "let in thunk assigns to the correct participant" $
        let
            globalType = GlobalType.globalType $ do
                GlobalType.transaction B C "thunk" 
                GlobalType.end

            localTypes = LocalType.projections globalType

            bob = do 
                thunk <- 
                    H.function $ \_ -> do
                        d <- H.create (VInt 42)
                        H.terminate

                H.send thunk 
                H.terminate

            carol = do 
                code <- H.receive 
                H.applyFunction code VUnit
                H.terminate

            state = 
                executionState Queue.empty 
                    [ ("B", createMonitor (id, localTypes Map.! "B") Map.empty, bob)
                    , ("C", createMonitor (id, localTypes Map.! "C") Map.empty, carol)
                    ]

            forwarded = 
                forwardN [ 2,2, 3,3,3,3 ]   state 

            bStore = 
                (_store . (Map.! "B") . participants ) <$> forwarded

            cStore = 
                (_store . (Map.! "C") . participants ) <$> forwarded

            thunk = 
                VFunction "var1" (Fix (Let "X" "var2" (VInt 42) (Fix NoOp)))

        in 
            1 `shouldBe` 1
            -- bStore `shouldBe`  Right (Map.fromList [("v0", thunk), ("v3",VInt 42)])
            -- cStore `shouldBe`  Right (Map.fromList [("v1", thunk), ("v2",VUnit)])

    it "receive adds its variable to the correct owner" $ 
        let 
            globalType = GlobalType.globalType $ do
                GlobalType.transaction C B "thunk" 
                GlobalType.transaction A C "fourtyTwo" 
                GlobalType.end

            localTypes = LocalType.projections globalType

            alice = do 
                H.send (VInt 42)
                H.terminate
    
            bob = do
                thunk <- H.receive
                H.applyFunction thunk VUnit
                H.terminate

            carol = do 
                thunk <- H.function $ \_ -> do
                    v <- H.receive
                    H.terminate

                H.send thunk
                H.terminate
                
            state = 
                executionState Queue.empty 
                    [ ("A", createMonitor (id, localTypes Map.! "A") Map.empty, alice)
                    , ("B", createMonitor (id, localTypes Map.! "B") Map.empty, bob)
                    , ("C", createMonitor (id, localTypes Map.! "C") Map.empty, carol)
                    ]

            aType = 
                (TypeContext.Sent "A" "C" "fourtyTwo", LocalType.end)

            bType = 
                (TypeContext.Application "B" "k0" . TypeContext.Received "B" "C" "thunk", LocalType.end)

            cType = 
                (TypeContext.Received "C" "A" "fourtyTwo" . TypeContext.Sent "C" "B" "thunk" . TypeContext.Assigned "C", LocalType.end)

            thunk = VFunction "var1" (Fix (Receive {owner = "C", variableName = "var2", continuation = Fix NoOp}))

            bStore = Map.fromList [ ("v1", thunk), ("v2", VUnit) ]
            cStore = Map.fromList [ ("v0", thunk) , ("v3", VInt 42) ]

            bobMonitor = 
                createMonitor bType  bStore 
                    |> Session.markVariableAsUsed "var0" "v1"
                    |> Session.addApplication ("k0",(VReference "v1",VUnit)) 
                    |> Session.markVariableAsUsed "v2" "v2"

            carolMonitor = 
                createMonitor cType cStore
                    |> Session.markVariableAsUsed "var0" "v0"
                    |> Session.markVariableAsUsed "var2" "v3"

            newState = 
                executionState (Queue.enqueueHistory ("A", "C", VInt 42) $ Queue.enqueueHistory ("C", "B", thunk) Queue.empty)
                    [ ("A", createMonitor aType Map.empty, H.terminate)
                    , ("B", bobMonitor, H.terminate)
                    , ("C", carolMonitor, H.terminate)
                    ]

        in forwardN [ 3,3, 1, 2,2,2,2 ] state `shouldBe` Right newState

    it "send behaves" $ 
        let monitor = createMonitor (id, LocalType.send "A" "A" "unit" LocalType.end) Map.empty
            state = executionState Queue.empty [("A", monitor, compileAlice $ H.send VUnit >> H.terminate)] 

            newMonitor = createMonitor (TypeContext.Sent "A" "A" "unit", LocalType.end) Map.empty
            newState = executionState 
                (Queue.enqueue ("A", "A", VUnit) Queue.empty) 
                [("A", newMonitor, H.terminate)]
        in
            forwarder state `shouldBe` Right newState 



    let sendReceiveProgram monitor1 monitor2 = 
            executionState Queue.empty
                [("A", monitor1, compileAlice $ H.send VUnit >> H.terminate) 
                ,("B", monitor2, compileBob $ do 
                    x <- H.receive 
                    H.terminate
                 )
                ]

    it "send/receive behaves" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty

            state = sendReceiveProgram monitor1 monitor2

            newMonitor1 = createMonitor (TypeContext.Sent "A" "B" "unit", LocalType.end) Map.empty
            newMonitor2 = 
                createMonitor 
                    (TypeContext.Received "B" "A" "unit", LocalType.end) 
                    (Map.singleton "v0" VUnit)
                    |> Session.markVariableAsUsed "var0" "v0"

            newState = executionState 
                (Queue.enqueueHistory ("A", "B", VUnit) Queue.empty) 
                [("A", newMonitor1, H.terminate)
                ,("B", newMonitor2, H.terminate)
                ]
        in
            forwardN [ 1,2 ] state `shouldBe` Right newState 

    it "receive errors when sender owner is incorrect" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "C" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "A" "unit" LocalType.end) Map.empty
            state = sendReceiveProgram monitor1 monitor2

            errorMessage = 
                QueueError "Receive" (Queue.InvalidBackwardQueueItem "receiver mismatch; the type expects B but the queue contains C")
        in
            forwardN [ 1,2 ] state `shouldBe` Left errorMessage 

    it "receive errors when sender owner is incorrect" $ 
        let monitor1 = createMonitor (id, LocalType.send    "A" "B" "unit" LocalType.end) Map.empty
            monitor2 = createMonitor (id, LocalType.receive "B" "C" "unit" LocalType.end) Map.empty
            state = sendReceiveProgram monitor1 monitor2

            errorMessage = 
                QueueError "Receive" (Queue.InvalidBackwardQueueItem "sender mismatch; the type expects C but the queue contains A")
        in
            forwardN [ 1,2 ] state `shouldBe` Left errorMessage 

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
                GlobalType.oneOf A B [ (,) "x" GlobalType.end , (,) "y" GlobalType.end ]

            type1 = LocalType.offer "A" "B" [ ("x",LocalType.end),("y",LocalType.end)]
            type2 = LocalType.select "B" "A" [("x",LocalType.end),("y",LocalType.end)]
                        
            monitor1 = createMonitor (id, type1) Map.empty
            monitor2 = createMonitor (id, type2) Map.empty

            state = offerSelectProgram monitor1 monitor2

            -- picked = Zipper ([],("x",Fix NoOp,LocalType.end),[("y",Fix NoOp, LocalType.end)])
            -- selection = Zipper ([],("x",VBool True,Fix NoOp,LocalType.end),[("y",VBool False,Fix NoOp,LocalType.end)])

            picked = Zipper ([],("x", LocalType.end),[("y", LocalType.end)])
            selection = Zipper ([],("x", LocalType.end),[("y", LocalType.end)])

            newMonitor1 = createMonitor (TypeContext.Offered "A" "B" picked, LocalType.end)    Map.empty

            newMonitor2 = createMonitor (TypeContext.Selected "B" "A" selection, LocalType.end) Map.empty

            newState = executionStateWithStack
                (Queue.enqueueHistory ("B", "A", VLabel "x") Queue.empty) 
                [("A", newMonitor1, [ OtherOffers $ Zipper ([],("x",Fix NoOp),[("y",Fix NoOp)]) ], H.terminate)
                ,("B", newMonitor2, [ OtherSelections $ Zipper ([],("x",VBool True,Fix NoOp),[("y",VBool False,Fix NoOp)]) ], H.terminate)
                ]
        in
            forwardN [ 2,1 ] state `shouldBe` Right newState 

    it "offer errors when selector owner is incorrect" $ 
        let globalType = 
                GlobalType.oneOf A B [ (,) "x" GlobalType.end , (,) "y" GlobalType.end ]

            type1 = LocalType.offer "A" "C" [ ("x",LocalType.end),("y",LocalType.end)]
            type2 = LocalType.select "B" "A" [("x",LocalType.end),("y",LocalType.end)]
                        
            monitor1 = createMonitor (id, type1) Map.empty
            monitor2 = createMonitor (id, type2) Map.empty

            state = offerSelectProgram monitor1 monitor2

            errorMessage =
                QueueError "Offer" (Queue.InvalidBackwardQueueItem "sender mismatch; the type expects C but the queue contains B")
        in
            forwardN [ 2,1] state `shouldBe` Left errorMessage

    it "offer errors when offerer owner is incorrect" $ 
        let globalType = 
                GlobalType.oneOf A B [ (,) "x" GlobalType.end , (,) "y" GlobalType.end ]

            type1 = LocalType.offer "A" "B" [ ("x",LocalType.end),("y",LocalType.end)]
            type2 = LocalType.select "B" "C" [("x",LocalType.end),("y",LocalType.end)]
                        
            monitor1 = createMonitor (id, type1) Map.empty
            monitor2 = createMonitor (id, type2) Map.empty

            state = offerSelectProgram monitor1 monitor2

            errorMessage =
                QueueError "Offer" (Queue.InvalidBackwardQueueItem "receiver mismatch; the type expects A but the queue contains C")
        in
            forwardN [ 2,1 ] state `shouldBe` Left errorMessage

testRenameVariable = describe "renameVariable" $ do
    let renamer = unFix . renameVariable "x" "y" . Fix

    it "renames function name" $
        renamer (Program.Application "A" (VReference "x") VUnit) `shouldBe` Program.Application "A" (VReference "y") VUnit

    it "renames reference in function argument" $
        renamer (Program.Application "A" (VReference "f") (VReference "x")) 
            `shouldBe` Program.Application "A" (VReference "f") (VReference "y")

    it "renames in assignment right-hand side" $
        renamer (Program.Let "A" "v" (VReference "x") Program.terminate)
            `shouldBe` Program.Let "A" "v" (VReference "y") Program.terminate

    it "renames in send payload right-hand side" $
        renamer (Program.Send "owner" (VReference "x") Program.terminate)
            `shouldBe` Program.Send "owner" (VReference "y") Program.terminate

    it "renames in IfThenElse condition" $
        renamer (Program.IfThenElse "owner" (VReference "x") Program.terminate Program.terminate)
            `shouldBe` Program.IfThenElse "owner" (VReference "y") Program.terminate Program.terminate

    it "renames in nested condition" $
        let lit name = Fix $ Program.Application "owner" (VReference name) VUnit
            example varname = Program.IfThenElse "owner" (VReference "q") (lit varname) (lit varname)
        in
            renamer (example "x") `shouldBe` example "y"

createMonitor :: (TypeContext.TypeContext String -> TypeContext.TypeContext String , LocalType String) 
              -> Map.Map String Value -> Monitor Value String
createMonitor (previous, localType) store = Monitor 
            { _localType = TypeContext.Unsynchronized (previous $ TypeContext.Hole,  localType) 
            , _store = store 
            , _applicationHistory = Map.empty
            , _recursiveVariableNumber = 0
            , _recursionPoints = []
            , _usedVariables = []
            }

executionState :: Queue.Queue Value 
               -> List (String, Monitor Value String, H.HighLevelProgram Void) 
               -> ExecutionState Value 
executionState queue values = 
    executionStateWithStack queue (List.map (\(a,b,c) -> (a,b,[],c)) values)

executionStateWithStack :: Queue.Queue Value 
               -> List (String, Monitor Value String, List OtherOptions, H.HighLevelProgram Void) 
               -> ExecutionState Value 
executionStateWithStack queue values = 
    let (participants, monitors, stacks, programs) = List.unzip4 values
    in
        ExecutionState
            { variableCount = 0
            , locationCount = List.length values
            , applicationCount = 0
            , participants = Map.fromList $ List.zip participants monitors 
            , locations = 
                zipWith H.compile participants programs
                    |> zip3 participants stacks
                    |> zipWith (\i program -> ("l" ++ show i, program)) [1..]
                    |> Map.fromList
            , queue = queue 
            , isFunction = \value -> 
                case value of 
                    VFunction arg body -> Just (arg, body)
                    _ -> Nothing

            }
