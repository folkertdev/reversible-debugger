{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts,StandaloneDeriving, UndecidableInstances, DeriveFunctor, NamedFieldPuns #-}   
module ReversibleLanguage ((|>),ReversibleLanguage.init, schedule, unschedule, reschedule, ThreadState(..), ExecutionState, Context(..), Interpreter, Progress(..), Thread(..), ReversibleLanguage(..), forward, backward, throw, catch) where

import Types
import Queue
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

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

type Interpreter value program = State.StateT (Context value) (Either Error) program


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


{-| Type to keep track of the progress that a thread makes 

Based on [a monad for deterministic parallelism](https://simonmar.github.io/bib/papers/monad-par.pdf)
-} 
data Progress work 
    = Done 
    | Step work 
    | Blocked work
    | Branched work work
    deriving (Show, Eq, Functor)


{-| An individual thread, with 

* a unique identifier
* list of history instructions 
* list of remaining program instructions
-}
data Thread a = Thread PID (List (History a)) (List a)  

type Threads a = Map PID (Thread a)

data ThreadState a = 
    ThreadState
        { active :: Threads a
        , inactive :: Threads a
        , blocked :: Threads a
        , filtered :: Threads a
        } 


{-| -} 
spawnChildThread :: PID -> a -> Threads a -> Threads a
spawnChildThread parentID value threads = 
    let 
        siblings = Map.keys threads 
            |> filter (\k -> length k == length parentID + 1 && parentID `isPrefixOf` k)
        childID = parentID ++ [ length siblings ]
    in
        Map.insert childID (Thread childID [] [value]) threads


schedule :: ReversibleLanguage program 
    => (Thread program -> Bool)
    -> Thread program 
    -> ThreadState program
    -> Interpreter (Value program) (Either (ThreadState program) (Thread program, ThreadState program))
schedule predicate thread@(Thread pid _ _) state@ThreadState{active, inactive, blocked, filtered} = 
    if predicate thread then do
        result <- forwardThread thread 
        case result of
            Done ->
                return $ reschedule (state { inactive = Map.insert pid thread inactive }) 

            Step newThread -> 
                return $ Right ( newThread, state ) 

            Blocked newThread -> 
                return $ reschedule (state { blocked = Map.insert pid newThread blocked })

            Branched parent child@(Thread childPID _ _) ->
                return $ Right ( parent, state { active = Map.insert childPID child active } )
    
    else
        return $ reschedule state 


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



reschedule :: ThreadState program -> Either (ThreadState program) (Thread program, ThreadState program)
reschedule state@ThreadState{active, inactive, blocked, filtered} =     
    case Map.minView active of
        Just (first, rest) -> 
            -- try to make progress on the minimal (most senior) thread 
            Right (first, state { active = rest }) 

        Nothing ->
            if Map.null blocked then
                -- finished all threads, give back the final state
                Left state
            else
                -- try to schedule the blocked threads again
                -- in the how that they are now unblocked
                reschedule (state { active = blocked, blocked = Map.empty })
        

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



