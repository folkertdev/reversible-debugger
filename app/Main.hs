module Main where

import System.Environment

import Interpreter
import Parser
import Types

import Control.Monad
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import qualified Data.Map as Map


main :: IO ()
main = do
    [ filename ] <- getArgs
    code <- readFile filename 

    let Right program = Parser.program code
    print program
    print "done parsing"


    ( identifierRef, bindings, threadNames, task) <- Interpreter.init program 


    let 
        interactive :: Task Program -> IOThrowsError ()
        interactive currentTask = do
            liftIO $ print $ activeInactiveThreads currentTask
            liftIO $ print currentTask
            command <- liftIO getLine
            case words command of 
                [ "x" ] ->
                    -- exit
                    return ()

                [ "f" ] -> 
                    interactive =<< forward identifierRef bindings threadNames currentTask

                [ "fthread", var ] -> 
                    interactive =<< forward identifierRef bindings threadNames currentTask


                [ "b" ] -> 
                    interactive =<< backward bindings threadNames currentTask

                [ "rollvariable", var ] ->
                    interactive =<< rollVariable (Identifier var) bindings threadNames currentTask

                [ "rollthread", var ] ->
                    interactive =<< rollThread (ThreadName var) bindings threadNames currentTask

                other -> do
                    liftIO $ print $ "unknown command: " ++ show other
                    interactive currentTask
                    

    result <- runExceptT $ interactive task
    print result

