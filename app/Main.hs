{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Main where

import System.Environment (getArgs)

-- import Interpreter
import DebuggerParser (Instruction(..), parse)
import MicroOz (Program, Value, History, init, forward, backward, rollThread)
import MicroOz.Parser as Parser
import Types
-- import ReversibleLanguage (schedule, reschedule, throw, ThreadState(..), ExecutionState, init, ReversibleLanguage)
--
import Data.Thread as Thread
import Data.ThreadState as ThreadState
import Data.Context as Context

import Control.Monad
import Control.Monad.Except as Except
import Control.Monad.Trans.State.Lazy as State 
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import qualified Data.Map as Map

import qualified Text.Show.Pretty as Pretty
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
            go $ ReplState context (Running thread ThreadState.OtherThreads{ active = Map.empty, inactive = Map.empty, blocked = Map.empty, filtered = Map.empty })




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
iteration state = do
    print "the state is"
    print "---"
    print state
    print "---"
    print "what is your command?"
    command <- getLine
    case DebuggerParser.parse command of 
        Left error -> do
            print error
            return $ Just state 

        Right instruction -> 
            interpretInstruction instruction state 

run thread threads = do
    stepped <- MicroOz.forward (Running thread threads)
    case stepped of 
        Stuck done -> return $ Left done
        Running t ts -> run t ts 


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

        mapOverThreads f = 
            case state of 
                Running thread threads ->
                    f thread threads
                
                Stuck _ ->
                    Except.throwError $ RuntimeException "cannot perform action on done threads"

    in
    case instruction of
        Forth pid ->
            evaluate $ MicroOz.forward state 

        Back pid-> 
            evaluate $ MicroOz.backward state 

        {-
        Roll pid n -> do
            result <- interpretInstruction (Back pid) state 
            case result of 
                Nothing -> 
                    return $ Just state  

                Just newState -> 
                    interpretInstruction (Roll pid (n - 1)) newState 
            
        RollSend channelName n -> 
            undefined

        RollReceive channelName n -> 
            undefined

        RollThread pid -> 
            undefined

        RollVariable identifier -> 
            undefined

        Run -> 
            helper $ mapOverThreads run
 

        ListThreads -> do
            print state
            return $ Just state 
            
        Store -> do
            print (getContext state)
            return $ Just state 
            
        Print id ->  
            undefined

        History id -> 
            undefined

        Help-> do 
            print "help stuff" 
            return $ Just state 

        -}
        Quit-> 
            return Nothing 
        
