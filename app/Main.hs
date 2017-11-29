{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Main where

import System.Environment (getArgs)

-- import Interpreter
import DebuggerParser (Instruction(..), parse)
import MicroOz (Program, Value, History, init, forward, backward, rollThread, scheduleThreadBackward, scheduleThread, rollSends, rollReceives, handleBackwardEffects)
import qualified MicroOz 
import MicroOz.Parser as Parser
import Types
-- import ReversibleLanguage (schedule, reschedule, throw, ThreadState(..), ExecutionState, init, ReversibleLanguage)
--
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

import Debug.Trace 

main :: IO ()
main = do
    [ path ] <- getArgs 
    contents <- readFile path
    case Parser.program contents of
        Left e -> 
            error (show e)
        Right program -> do 
            let ( context, thread ) = MicroOz.init program
            go $ ReplState context (ThreadState.singleton thread) 




go :: ReplState -> IO () 
go state = do
    stepped <- iteration state 
    mapM_ go stepped

{-

repeatedApplication n x = foldl (>=>) return $ replicate n x

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
-}

data ReplState = ReplState (Context Value) (ThreadState History Program) deriving (Show)


getContext :: ReplState -> Context Value
getContext (ReplState context state) = 
    context
    

iteration :: ReplState -> IO (Maybe ReplState)
iteration state@(ReplState context threads) = do
    putStr "\n========= \n\n"
    print threads
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

run :: (MonadState (Context Value) m, MonadError Error m) => ThreadState History Program -> m (ThreadState History Program)
run state= 
    case state of 
        Stuck done -> return $ Stuck done
        Running t ts -> 
            run =<< MicroOz.forward state 

andThen :: Monad m => (a -> m b) -> m a -> m b
andThen = (=<<)

interpretInstruction :: Instruction -> ReplState -> IO (Maybe ReplState) 
interpretInstruction instruction (ReplState context state) =  
    let 

        evaluate :: StateT (Context Value) (Either Error) (ThreadState History Program) -> IO (Maybe ReplState)
        evaluate computation = 
            case runStateT computation context of 
                Left error -> do
                    print error
                    return $ Just $ ReplState context state 

                Right (a, s) ->
                    return . Just $ ReplState s a
    in
    case instruction of
        Forth pid ->
            MicroOz.scheduleThread pid state
                |> andThen MicroOz.forward
                |> evaluate

        Back pid -> 
            MicroOz.scheduleThreadBackward pid state
                |> andThen MicroOz.backward
                |> evaluate

        Roll pid n -> 
            case runStateT (MicroOz.scheduleThreadBackward pid state >>= MicroOz.backward) context of 
                Left (SchedulingError e) -> do
                    print e 
                    return $ Just (ReplState context state)

                Left e -> do
                    print e 
                    return $ Just (ReplState context state)
                
                Right (newState, newContext) -> 
                    interpretInstruction (Roll pid (n - 1)) (ReplState newContext newState) 

            
        RollSend channelName n -> 
            evaluate $ MicroOz.rollSends n channelName state 
                

        RollReceive channelName n -> 
            evaluate $ MicroOz.rollReceives n channelName state 


        RollThread pid -> 
            evaluate $ MicroOz.handleBackwardEffects MicroOz.RollThread{ MicroOz.caller = PID.parent pid, MicroOz.toRoll = pid} state

        RollVariable identifier -> 
            undefined

        Run -> 
            evaluate $ run state
 

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
        
