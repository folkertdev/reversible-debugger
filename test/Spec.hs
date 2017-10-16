
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad


import Interpreter
import Types
import Parser


import Control.Monad.Trans.State as State 

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "assert parses correctly" $ do
        Parser.program "assert (5 >= 10) end" `shouldBe` Right (Assert (Operator GreaterThanEqual (IntValue 5) (IntValue 10 )))

    it "assert throws an assertion error" $
        let 
            condition = Operator GreaterThanEqual (IntValue 5) (IntValue 10 )

            program = Assert condition

            error = AssertionError condition

            ( context, task ) = Interpreter.init program
        in

            runStateT (forward task) context `shouldBe` Left error

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

    it "Sequence is preserved when moving forward and then backward" $ do 
        let program = Sequence Skip (Let (Identifier "a") Port Skip)
            
            ( context, task ) = Interpreter.init program

        runStateT (forward task >>= backward) context `shouldBe` Right (task, context)
            
    it "Let is preserved when moving forward and then backward" $ do 
        let program = Let (Identifier "var1") Port Skip
            
            ( context, task ) = Interpreter.init program

        runStateT (forward task >>= backward) context `shouldBe` Right (task, context)

    it "If on True is preserved when moving forward and then backward" $ 
        let program = If condition Skip (Sequence Skip Skip) 

            condition = Operator GreaterThanEqual (IntValue 15) (IntValue 10 )
        in
            forwardBackwardIsIdentity 1 program
            

    it "If on False is preserved when moving forward and then backward" $ 
        let program = If condition Skip (Sequence Skip Skip) 

            condition = Operator GreaterThanEqual (IntValue 5) (IntValue 10 )
        in
            forwardBackwardIsIdentity 1 program
            
    it "Function application is preserved when moving forward and then backward" $ 
        let program = Let (Identifier "var1") Port $ Let (Identifier "var2") (Procedure [Identifier "y" ] $ If condition Skip (Sequence Skip Skip)) 
                $ Apply (Identifier "var2") [ Identifier "x" ]

            condition = Operator GreaterThanEqual (IntValue 5) (IntValue 10 )
        in
            forwardBackwardIsIdentity 7 program

    it "Receive is preserved when moving forward and then backward" $ 
        let program = 
                Let (Identifier "var1") Port $ 
                Let (Identifier "var2") Port $ 
                        Sequence (Send (Identifier "var1") (Identifier "var2")) $ 
                        Let (Identifier "var3") (Receive $ Identifier "var1") $ Skip  

            condition = Operator GreaterThanEqual (IntValue 5) (IntValue 10 )
        in
            forwardBackwardIsIdentity 7 program




forwardBackwardIsIdentity n program =
    let 
        ( context, task ) = Interpreter.init program
    
        f = foldl (>=>) return (replicate n forward)
        b = foldl (>=>) return (replicate n backward)
    in
        runStateT (f task >>= b) context `shouldBe` Right (task, context)
