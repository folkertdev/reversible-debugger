module Main where

import System.Environment

import Interpreter
import Parser
import Types

import Control.Monad
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State.Lazy as State 
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import qualified Data.Map as Map


main :: IO ()
main = runInterpreter 
        

repeatedApplication n x = foldl (>=>) return $ replicate n x

run :: Context -> Interpreter a -> IO (Context, a)
run context computation = 
    case runStateT computation context of
        Left e -> do
            print $ show context 
            error (show e)

        Right (a, s) -> 
            return (s, a)

runInterpreter :: IO () 
runInterpreter = do
    [ filename ] <- getArgs
    code <- readFile filename 

    let parseResult  = Parser.program code
    case parseResult of
        Left error ->
            print error
        Right program -> do
            let ( context, task) = Interpreter.init program 


            let 
                interactive :: Context -> Task Program -> IO Context
                interactive context currentTask = do
                    print $ activeInactiveThreads currentTask
                    print currentTask
                    command <- getLine
                    case words command of 
                        [ "x" ] ->
                            -- exit
                            return context

                        [ "f" ] -> 
                            uncurry interactive =<< run context (forward currentTask)

                        [ "ff" ] -> 
                            uncurry interactive =<< run context (repeatedApplication 10 forward currentTask)

                        [ "fthread", var ] -> 
                            uncurry interactive =<< run context (forward currentTask)

                        [ "b" ] -> 
                            uncurry interactive =<< run context (backward currentTask)

                        [ "rollvariable", var ] ->
                            uncurry interactive =<< run context (rollVariable (Identifier var) currentTask)

                        [ "rollthread", var ] ->
                            uncurry interactive =<< run context (rollThread (ThreadName var) currentTask)

                        other -> do
                            liftIO $ print $ "unknown command: " ++ show other
                            interactive context currentTask
                            

            result <- interactive context task
            print result


