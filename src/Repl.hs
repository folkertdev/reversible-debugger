module Repl (run) where 

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
import Data.ReplState as ReplState

import Control.Monad
import Control.Monad.Except as Except
import Control.Monad.State as State 
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar 
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Map as Map

import Debug.Trace as Debug


runner :: Execution () 
runner = go 30 
  where go 0 = return () 
        go n = do
            (context, state) <- State.get
            case Debug.traceShowId $ ThreadState.reschedule state of
                Nothing -> return ()
                Just newState -> do
                    State.modify (\(c, _) -> (Debug.traceShowId c, newState ))
                    Interpreter.forward
                    go (n - 1)


run = interpretInstruction Run 

interpretInstruction :: Instruction -> ReplState -> Either String ReplState 
interpretInstruction instruction (ReplState context state) =  
    let 
        evaluate :: StateT (Context Value, ThreadState History Program) (Either Error) a -> Either String ReplState 
        evaluate computation = 
            case State.runStateT computation (context, state) of
                Left e -> do
                    Except.throwError (show e)

                Right (_, ( newContext, newState)) -> 
                    return $ ReplState newContext newState  

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
                Left e -> 
                    Except.throwError (show e)
                
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
            evaluate runner 

        SkipLets -> 
            evaluate Interpreter.skipLets
 
        ListThreads -> do
            undefined
            
        Store -> do
            undefined
            
        Print id ->  
            undefined

        History id -> 
            undefined

        Help-> do 
            undefined

        Quit-> 
            undefined
        
