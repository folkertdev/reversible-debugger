module Main where

import System.Environment

import Interpreter
import MicroOz
import MicroOz.Parser as Parser
import Types
import ReversibleLanguage (executeWhile)

import Control.Monad
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State.Lazy as State 
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import qualified Data.Map as Map

import qualified Text.Show.Pretty as Pretty

main :: IO ()
main = runInterpreter 
        

repeatedApplication n x = foldl (>=>) return $ replicate n x

run :: Show v => Context v -> Interpreter v a -> IO (Context v, a)
run context computation = 
    case runStateT computation context of
        Left e -> do
            print $ show context 
            error (show e)

        Right (a, s) -> 
            return (s, a)

skipLets :: Task (Thread Program) -> Interpreter (Value Program) (Task (Thread Program))
skipLets task = 
    let predicate (Thread _ _ instructions) = 
            case instructions of 
                (Let _ _ _ : _) -> True
                _ -> False
    in
    -- find next instruction, but don't execute it
    executeWhile predicate task

runInterpreter :: IO () 
runInterpreter = do
    [ filename ] <- getArgs
    code <- readFile filename 

    let parseResult  = Parser.program code
    case parseResult of
        Left error ->
            print error
        Right program -> do
            let ( context, task) = MicroOz.init program 


            let 
                interactive :: Context (Value Program) -> Task (Thread Program) -> IO (Context (Value Program))
                interactive context currentTask = do
                    print $ activeInactiveThreads currentTask
                    Pretty.pPrint currentTask
                    command <- getLine
                    case words command of 
                        [ "x" ] ->
                            -- exit
                            return context

                        [ "f" ] -> 
                            uncurry interactive =<< run context (forward currentTask)

                        [ "skiplets" ] -> 
                            -- advances the program until the next statement that is not a Let
                            uncurry interactive =<< run context (skipLets currentTask)

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


