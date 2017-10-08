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

x =
    Parallel
        (Thread (ThreadName "t_0")
            [ HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, HistoryEsc, SpawnedThread (ThreadName "t_0_4"), HistoryEsc, SpawnedThread (ThreadName "t_0_3"), Composed, HistoryEsc, SpawnedThread (ThreadName "t_0_2"), Composed, HistoryEsc, SpawnedThread (ThreadName "t_0_1"), Composed, CreatedVariable (Identifier "var16"), CreatedVariable (Identifier "var15"), CreatedVariable (Identifier "var14"), CreatedVariable (Identifier "var13"), CreatedVariable (Identifier "var12"), CreatedVariable (Identifier "var11"), CreatedVariable (Identifier "var10"), CreatedVariable (Identifier "var9"), CreatedVariable (Identifier "var8"), CreatedVariable (Identifier "var7"), CreatedVariable (Identifier "var6"), CreatedVariable (Identifier "var5"), CreatedVariable (Identifier "var4"), CreatedVariable (Identifier "var3"), CreatedVariable (Identifier "var2"), CreatedVariable (Identifier "var1") ]
            []
        )
        (Map.fromList
            [ ( ThreadName "t_0_1", Singleton (Thread (ThreadName "t_0_1") [] [ Apply (Identifier "var12") [ Identifier "var6", Identifier "var7", Identifier "var8" ] ]) )
            , ( ThreadName "t_0_2", Singleton (Thread (ThreadName "t_0_2") [] [ Sequence (Send (Identifier "var7") (Identifier "var9")) (Apply (Identifier "var15") [ Identifier "var9" ]) ]) )
            , ( ThreadName "t_0_3", Singleton (Thread (ThreadName "t_0_3") [ HistoryEsc, HistoryEsc, Sent (Identifier "var11"), CalledProcedure (Identifier "var16") [ Identifier "var11" ], HistoryEsc, Sent (Identifier "var7"), Composed ] []) )
            , ( ThreadName "t_0_4", Singleton (Thread (ThreadName "t_0_4") [ CalledProcedure (Identifier "var14") [ Identifier "var10" ], HistoryEsc, Sent (Identifier "var7"), Composed ] [ If (Operator GreaterThan (IntIdentifier (Identifier "c")) (IntIdentifier (Identifier "p"))) (Send (Identifier "var10") (Identifier "var1")) (Apply (Identifier "var13") [ Identifier "var10" ]), Esc, Esc ]) )
            ]
        )

main :: IO ()
main = runInterpreter 
        

repeatedApplication n x = foldl (>=>) return $ replicate n x

test = do

    print x
    ( identifierRef, bindings, threadNames, task) <- Interpreter.init Skip

    stepped <- runExceptT $ repeatedApplication 30 (forward identifierRef bindings threadNames) x

    print stepped

runInterpreter :: IO () 
runInterpreter = do
    [ filename ] <- getArgs
    code <- readFile filename 

    let parseResult  = Parser.program code
    case parseResult of
        Left error ->
            print error
        Right program -> do
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

                        [ "ff" ] -> 
                            interactive =<< repeatedApplication 10 (forward identifierRef bindings threadNames) currentTask

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


