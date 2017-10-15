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

run :: Context -> MonadInterpreter a -> IO (Context, a)
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


