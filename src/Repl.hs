module Repl (interpretInstruction, instructionDirection) where 

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
import qualified Data.Direction as Direction

import Control.Monad
import Control.Monad.Except as Except
import Control.Monad.State as State 
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Map as Map


instructionDirection :: Instruction -> Maybe Direction.Direction 
instructionDirection instruction = 
    case instruction of 
        Forth _ -> 
            Just Direction.forward 

        Run -> 
            Just Direction.forward 

        Back _ -> Just Direction.backward
        RollVariable _ -> Just  Direction.backward
        RollThread _ -> Just  Direction.backward
        RollSend channelName n -> Just   Direction.backward
        RollReceive channelName n -> Just   Direction.backward 

        _ -> 
            Nothing

interpretInstruction :: Instruction -> ReplState -> Either Error ReplState 
interpretInstruction instruction (ReplState context state) =  
    let 
        evaluate :: StateT (Context Value, ThreadState History Program) (Either Error) a -> Either Error ReplState 
        evaluate computation = 
            case State.runStateT computation (context, state) of
                Left e -> 
                    Except.throwError e

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
                    Except.throwError e
                
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
            undefined -- evaluate runner 

        SendReceiveNormalForm direction -> 
            evaluate (Interpreter.sendReceiveNormalForm direction)
 
        ListThreads ->
            undefined
            
        Store -> 
            undefined
            
        Print id ->  
            undefined

        History id -> 
            undefined

        Help-> 
            undefined

        Quit-> 
            undefined
        
