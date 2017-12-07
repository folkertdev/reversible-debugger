{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Main where

import System.Environment (getArgs)

import DebuggerParser (Instruction(..), parse)
import qualified Interpreter
import Interpreter (Execution) 
import qualified MicroOz 
import MicroOz (Program, Value, History, init) 
import MicroOz.Parser as Parser
import Types

import Data.Thread as Thread
import Data.ThreadState as ThreadState
import Data.Context as Context
import Data.PID as PID

import Control.Monad
import Control.Monad.Except as Except
import Control.Monad.State as State 
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Map as Map

import Debug.Trace as Debug


main :: IO ()
main = do
    [ path ] <- getArgs 
    interpreter path


data ReplState = ReplState (Context Value) (ThreadState History Program) deriving (Show)


interpreter path = do
    contents <- readFile path
    case Parser.programWithTypes contents of
        Left e -> 
            error (show e)
        Right (types, program) -> do 
            let ( context, thread ) = MicroOz.init types program
            go $ ReplState context (ThreadState.singleton thread) 


go :: ReplState -> IO () 
go state@(ReplState context _) = do
    stepped <- iteration state 
    mapM_ go stepped


iteration :: ReplState -> IO (Maybe ReplState)
iteration state@(ReplState context threads) = do
    putStr "\n========= \n\n"
    -- print threads
    putStr "\n"
    putStr "command: "
    command <- getLine
    case DebuggerParser.parse command of 
        Left error -> do
            print error
            return $ Just state 

        Right instruction -> do
            putStr "parsed: "
            print instruction
            interpretInstruction instruction state 

run :: Execution () 
run = go 10 
  where go 0 = return () 
        go n = do
            (context, state) <- State.get
            case Debug.traceShowId $ ThreadState.reschedule state of
                Nothing -> return ()
                Just newState -> do
                    State.modify (\(c, _) -> (Debug.traceShowId c, newState ))
                    Interpreter.forward
                    go (n - 1)


interpretInstruction :: Instruction -> ReplState -> IO (Maybe ReplState) 
interpretInstruction instruction (ReplState context state) =  
    let 
        evaluate :: StateT (Context Value, ThreadState History Program) (Either Error) a -> IO (Maybe ReplState)
        evaluate computation = 
            case State.runStateT computation (context, state) of
                Left e -> do
                    print e 
                    return $ Just (ReplState context state)

                Right (_, ( newContext, newState)) -> 
                    return . Just $ ReplState newContext newState  

    in
    case instruction of
        Forth pid -> evaluate $ do
            Interpreter.scheduleThread pid 
            Interpreter.forward
                

        Back pid -> evaluate $ do 

            Interpreter.scheduleThreadBackward pid 
            Interpreter.backward

        Roll pid n ->
            case runStateT (Interpreter.scheduleThreadBackward pid >> Interpreter.backward) (context, state) of 
                Left e -> do
                    print e 
                    return $ Just (ReplState context state)
                
                Right ((), (newContext, newState)) -> 
                    interpretInstruction (Roll pid (n - 1)) (ReplState newContext newState) 
            
        RollSend channelName n -> 
            evaluate $ Interpreter.rollSends n channelName  
                

        RollReceive channelName n -> 
            evaluate $ Interpreter.rollReceives n channelName  


        RollThread pid -> 
            evaluate $ Interpreter.rollThread pid 

        RollVariable identifier -> 
            evaluate $ Interpreter.rollVariable identifier

        Run -> 
            evaluate run 

        SkipLets -> 
            evaluate Interpreter.skipLets
 
        ListThreads -> do
            print $ Context.threads context
            return $ Just (ReplState context state) 
            
        Store -> do
            print context 
            return $ Just (ReplState context state) 
            
        Print id ->  
            undefined

        History id -> 
            undefined

        Help-> do 
            print "help stuff" 
            return $ Just (ReplState context state) 

        Quit-> 
            return Nothing 
        
