
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


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
