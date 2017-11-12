module Main where

import System.Environment

import Interpreter
import MicroOz
import MicroOz.Parser as Parser
import Types
import ReversibleLanguage (schedule, reschedule, ThreadState(..), ExecutionState, init)

import Control.Monad
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State.Lazy as State 
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import qualified Data.Map as Map

import qualified Text.Show.Pretty as Pretty
import Debug.Trace 

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

skipLets :: Thread Program -> ThreadState Program -> Interpreter (Value Program) (Either (ThreadState Program) (Thread Program, ThreadState Program))
skipLets thread threads = 
    let predicate (Thread _ _ instructions) = 
            case instructions of 
                (Let _ _ _ : _) -> True
                _ -> False
    in do
        oneIteration <- schedule predicate thread threads
        case oneIteration of 
            Left done -> return $ Right (thread, threads) 
            Right (t, ts) -> skipLets t ts

runInterpreter :: IO () 
runInterpreter = do
    [ filename ] <- getArgs
    code <- readFile filename 

    let parseResult  = Parser.program code
    case parseResult of
        Left error ->
            print error
        Right program -> do
            let ( context, thread) = MicroOz.init program 
                
            let 
                interactive :: Context (Value Program) -> Either (ThreadState Program) (Thread Program, ThreadState Program) -> IO (Context (Value Program))
                interactive context currentTask = 
                    case currentTask of
                        Left done -> return context 
                        Right ( currentThread, otherThreads) -> do
                            -- print $ activeInactiveThreads currentThread otherThreads
                            Pretty.pPrint currentThread 
                            command <- getLine
                            case words command of 
                                [ "x" ] ->
                                    -- exit
                                    return context

                                [ "f" ] -> 
                                    uncurry interactive =<< run context (forward currentThread otherThreads)

                                [ "skiplets" ] -> 
                                    -- advances the program until the next statement that is not a Let
                                    uncurry interactive =<< run context (skipLets currentThread otherThreads)

                                ( "fthread": var ) -> do
                                    let pid = map read var
                                    print $ "forwarding thread " ++ show pid
                            
                                    uncurry interactive =<< run context (advanceThread pid currentThread otherThreads)

                                [ "b" ] -> 
                                    uncurry interactive =<< run context (backward currentThread otherThreads)

                                [ "rollvariable", var ] ->
                                    uncurry interactive =<< run context (rollVariable (Identifier var) currentThread otherThreads)

                                ("rollthread": var)  -> do
                                    let pid = map read var
                                    print $ "unrolling thread " ++ show pid
                            
                                    uncurry interactive =<< run context (rollThread pid currentThread otherThreads)

                                other -> do
                                    liftIO $ print $ "unknown command: " ++ show other
                                    interactive context currentTask
                                    

            result <- interactive context (Right (thread , ThreadState Map.empty Map.empty Map.empty Map.empty))
            print result


