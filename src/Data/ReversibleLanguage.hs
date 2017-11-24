{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts,StandaloneDeriving, UndecidableInstances, DeriveFunctor, NamedFieldPuns #-}   
module Data.ReversibleLanguage ((|>),ReversibleLanguage.init, schedule, unschedule, ThreadState(..), ExecutionState, Context(..), Interpreter, Progress(..), ReversibleLanguage(..), forward, backward, throw, catch) where

import Types
import Queue
import qualified Data.Thread as Thread

import Data.Semigroup (Semigroup(..)) 
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State as State 
import Control.Applicative (Applicative, liftA2, pure, (<*))
import Control.Monad
import Data.Foldable (Foldable, foldrM, asum, find)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)



type Thread a = Thread.Thread (History a) a


data Context a = 
    Context 
        { _threads :: Map PID Int
        , _variableCount :: Int
        , _bindings :: Map Identifier a
        , _channels :: Map Identifier (Queue.Queue Identifier)
        }
    deriving (Show)


class (Eq a, Show a, Show (Value a), Show (History a), Eq (Value a), Eq (History a)) => ReversibleLanguage a where 
    -- associated value type
    data Value a :: * 

    -- associated history type
    data History a :: * 

    -- moving individual threads
    forwardThread  :: Thread a -> Interpreter (Value a) (Progress  (Thread a))
    backwardThread :: Thread a -> Interpreter (Value a) (Progress  (Thread a))

    -- the spawn instruction of the language
    -- needed for handling the Branched case of Progress
    spawn :: a -> a

    -- history inspection for correct rolling of threads and variables
    spawned :: History a -> Maybe PID
    createdVariable :: History a -> Maybe Identifier
    sent :: History a -> Maybe Identifier
    received :: History a -> Maybe Identifier





unschedule :: ReversibleLanguage program 
    => (Thread program -> Bool)
    -> Thread program 
    -> ThreadState program
    -> Interpreter (Value program) (Either (ThreadState program) (Thread program, ThreadState program))
unschedule predicate thread@(Thread pid history program) state@ThreadState{active, inactive, blocked, filtered} = 
    if predicate thread then 
        case history of
            [] -> 
                return $ reschedule (state { inactive = Map.insert pid thread inactive }) 

            (mostRecent:rest) -> 
                case spawned mostRecent of
                    Nothing -> do
                        -- reverse the parent thread, keeping the children constant
                        newParent <- backwardThread thread 
                        case newParent of
                            Done ->
                                return $ reschedule (state { inactive = Map.insert pid thread inactive }) 

                            Step newThread -> 
                                return $ Right ( newThread, state ) 

                            Blocked _ -> 
                                error "backwardThread blocked"

                            Branched _ _ -> 
                                error "backwardThread branched"


                    Just spawnedName -> 
                       case Map.lookup spawnedName active of
                            Nothing -> 
                               case Map.lookup spawnedName inactive of
                                   Just (Thread _ _ [childProgram]) -> do
                                       -- child is already completely rolled 
                                       State.modify $ \context -> 
                                            let 
                                                newThreads = Map.adjust (\v -> v - 1 :: Int) pid $ Map.delete spawnedName (_threads context)
                                            in
                                                context { _threads = newThreads } 

                                       return $ Right ( Thread pid rest (spawn childProgram : program), state ) 

                                   other -> 
                                       error $ "child is invalid" ++ show other

                            Just childThread -> 
                                schedule predicate childThread (state { active = Map.insert pid thread active })
                                 

    else
        return $ reschedule state 



deriving instance (Eq a, Eq (History a)) => Eq (Thread a) 
deriving instance (Show a, Show (History a)) => Show (Thread a) 


throw :: Error -> StateT s (Either Error) a
throw error = State.StateT (\s -> Left error)


type ExecutionState a = Either  (ThreadState a) (Thread a, ThreadState a)

{-| Evaluate a program one step forward -} 
forward :: ReversibleLanguage program => Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program)
forward = 
    schedule (const True) 

init :: ReversibleLanguage program => Thread program -> Interpreter (Value program) (ExecutionState program)
init thread = 
    schedule (const True) thread (ThreadState Map.empty Map.empty Map.empty Map.empty)


-- Move Backward

backward :: ReversibleLanguage program => Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program)
backward thread@(Thread name history program) =
    unschedule (const True) thread 


catch :: Interpreter v a -> (Error -> Interpreter v a) -> Interpreter v a
catch tryBlock handler = do
    context <- State.get
    case runStateT tryBlock context of
        Right ( value, newContext ) -> do
            put newContext
            return value

        Left error -> 
            handler error



