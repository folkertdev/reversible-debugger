module Main where

import System.Environment

import Interpreter
import DebuggerParser (Instruction(..), parse)
import MicroOz
import MicroOz.Parser as Parser
import Types
import ReversibleLanguage (schedule, reschedule, throw, ThreadState(..), ExecutionState, init)

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
            _
        RollReceive channelName n -> 
            _
        RollThread pid -> 
            helper $ mapOverThreads (rollThread pid) 

        RollVariable identifier -> 
            helper $ mapOverThreads (rollVariable identifier) 

        Run -> 
            _
 

        ListThreads -> do
            print state
            return $ Just state 
            
        Store -> do
            print (getContext state)
            return $ Just state 
            
        Print id ->  
            _
        History id -> 
            _
        Help-> do 
            print "help stuff" 
            return $ Just state 

        Quit-> 
            return Nothing 

