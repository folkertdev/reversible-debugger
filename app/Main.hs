{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Main where

main = return () 

{-
import System.Environment

import Interpreter
import DebuggerParser (Instruction(..), parse)
import MicroOz
import MicroOz.Parser as Parser
import Types
import ReversibleLanguage (schedule, reschedule, throw, ThreadState(..), ExecutionState, init, ReversibleLanguage)

import Control.Monad
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
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
            go $ Active context thread ThreadState{ active = Map.empty, inactive = Map.empty, blocked = Map.empty, filtered = Map.empty }




go :: ReplState Program -> IO () 
go state = do
    stepped <- iteration state 
    mapM_ go stepped


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

data ReplState program 
    = Done (Context (Value program)) (ThreadState program)
    | Active (Context (Value program)) (Thread program) (ThreadState program)

deriving instance (ReversibleLanguage program) => Show (ReplState program) 

getContext :: ReplState program -> Context (Value program)
getContext state = 
    case state of
        Done context _ -> context
        Active context _ _ -> context
    

iteration :: ReplState Program -> IO (Maybe (ReplState Program))
iteration state = do
    command <- getLine
    case DebuggerParser.parse command of 
        Left error -> do
            print error
            return $ Just state 

        Right instruction -> 
            interpretInstruction instruction state 

run thread threads = do
    stepped <- forward thread threads
    case stepped of 
        Left done -> return $ Left done
        Right (t, ts) -> run t ts 

interpretInstruction :: Instruction -> ReplState Program -> IO (Maybe (ReplState Program)) 
interpretInstruction instruction state =  
    let 

        helper :: Interpreter (Value Program) (ExecutionState Program) -> IO (Maybe (ReplState Program)) 
        helper computation = 
            case runStateT computation (getContext state) of 
                Left error -> do
                    print error
                    return $ Just state 

                Right (a, s) ->
                    return . Just $ 
                        case a of
                            Left threads -> 
                                Done s threads 

                            Right (current, other) ->
                                Active s current other

        mapOverThreads f = 
            case state of 
                Done _ threads -> 
                    throw $ RuntimeException "cannot perform action on done threads"

                Active _ thread threads -> 
                    f thread threads
                     

    in
    case instruction of
        Forth pid-> 
            helper $ mapOverThreads (advanceThread pid)

        Back pid-> 
            helper $ mapOverThreads backward 

        Roll pid n -> do
            result <- interpretInstruction (Back pid) state 
            case result of 
                Nothing -> 
                    return $ Just state  

                Just newState -> 
                    interpretInstruction (Roll pid (n - 1)) newState 
            
        RollSend channelName n -> 
            helper $ mapOverThreads (Interpreter.rollSend channelName)

        RollReceive channelName n -> 
            helper $ mapOverThreads (Interpreter.rollReceive channelName)

        RollThread pid -> 
            helper $ mapOverThreads (rollThread pid) 

        RollVariable identifier -> 
            helper $ mapOverThreads (rollVariable identifier) 

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

        Quit-> 
            return Nothing 

-}
