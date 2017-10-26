 {-# LANGUAGE ScopedTypeVariables , TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module ReversibleLanguage (schedule, ThreadState, ExecutionState, Context(..), Interpreter, Progress(..), Task(..), Thread(..), ReversibleLanguage(..), forward, backward, throw, executeWhile, catch) where

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



type Threads a = Map PID (Thread a)


data Context a = 
    Context 
        { _threads :: Map PID Int
        , _variableCount :: Int
        , _bindings :: Map Identifier a
        , _channels :: Map Identifier (Queue.Queue Identifier)
        }
    deriving (Show)


type Interpreter value program = State.StateT (Context value) (Either Error) program


class (Eq a, Show a, Show (Value a), Show (History a), Eq (Value a), Eq (History a)) => ReversibleLanguage a where 
    data Value a :: * 
    data History a :: * 
    forwardThread  :: Thread a -> Interpreter (Value a) (Progress  (Thread a))
    backwardThread :: Thread a -> Interpreter (Value a) (Progress  (Thread a))

    spawn :: a -> a

    spawned :: History a -> Maybe PID
    createdVariable :: History a -> Maybe Identifier



data Progress work 
    = Done work
    | Step work 
    | Blocked work
    | Branched work work
    deriving (Show, Eq, Functor)

data Task a 
    = Parallel a (List (Task a))
    deriving (Functor, Foldable, Traversable)
    

data Thread a = Thread PID (List (History a)) (List a)  

spawnThread :: PID -> a -> Threads a -> Threads a
spawnThread parentID value threads = 
    let 
        siblings = Map.keys threads 
            |> filter (\k -> length k == length parentID + 1 && parentID `isPrefixOf` k)
        childID = parentID ++ [ length siblings ]
    in
        Map.insert childID (Thread childID [] [value]) threads


type ThreadState a = ( Threads a, Threads a, Threads a, Threads a)

schedule :: ReversibleLanguage program 
    => (Thread program -> Bool)
    -> Thread program 
    -> ThreadState program
    -> Interpreter (Value program) (Either (ThreadState program) (Thread program, ThreadState program))
schedule predicate thread@(Thread pid _ _) (active, inactive, blocked, filtered) = 
    if predicate thread then do
        result <- forwardThread thread 
        case result of
            Done newThread ->
                return $ reschedule (active, Map.insert pid newThread inactive, blocked, filtered)

            Step newThread -> 
                return $ Right ( newThread, (active, inactive, blocked, filtered))

            Blocked newThread -> 
                return $ reschedule (active, inactive, Map.insert pid newThread blocked, filtered)

            Branched parent child@(Thread childPID _ _) ->
                return $ Right ( parent, (Map.insert childPID child active, inactive, blocked, filtered))
    else
        return $ reschedule ( active, inactive, blocked, filtered )


unschedule :: ReversibleLanguage program 
    => (Thread program -> Bool)
    -> Thread program 
    -> ThreadState program
    -> Interpreter (Value program) (Either (ThreadState program) (Thread program, ThreadState program))
unschedule predicate thread@(Thread pid history program) (active, inactive, blocked, filtered) = 
    if predicate thread then 
        case history of
            [] -> return $ reschedule (active, Map.insert pid thread inactive, blocked, filtered)
            (mostRecent:rest) -> 
                case spawned mostRecent of
                    Nothing -> do
                        -- reverse the parent thread, keeping the children constant
                        newParent <- backwardThread thread 
                        case newParent of
                            Done newThread ->
                                return $ reschedule (active, Map.insert pid newThread inactive, blocked, filtered)

                            Step newThread -> 
                                return $ Right ( newThread, (active, inactive, blocked, filtered))

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

                                       return $ Right ( Thread pid rest (spawn childProgram : program), (active, inactive, blocked, filtered))

                                   other -> 
                                       error $ "child is invalid" ++ show other

                            Just childThread -> 
                              schedule predicate childThread (Map.insert pid thread active, inactive, blocked, filtered) 
                                 

    else
        return $ reschedule ( active, inactive, blocked, filtered )



reschedule :: ThreadState program -> Either (ThreadState program) (Thread program, ThreadState program)
reschedule state@( active, inactive, blocked, filtered ) = 
    case Map.minView active of
        Just (first, rest) -> 
            -- try to make progress on the minimal (most senior) thread 
            Right (first, (rest, inactive, blocked, filtered))

        Nothing ->
            if Map.null blocked then
                -- finished all threads, give back the final state
                Left state
            else
                -- try to schedule the blocked threads again
                -- in the how that they are now unblocked
                reschedule ( blocked, inactive, Map.empty, filtered )
        

deriving instance (Eq a, Eq (History a)) => Eq (Thread a) 
deriving instance (Show a, Show (History a)) => Show (Thread a) 

deriving instance Show a => Show (Task a)
deriving instance Eq a => Eq (Task a)




joinTask :: Task (Task a) -> Task a
joinTask task =
    case task of 
        Parallel parent children -> 
            case parent of
                Parallel node subchildren ->
                    Parallel node (subchildren <> fmap joinTask children)

instance Applicative Task where
    pure = return
    (<*>) = ap

instance Monad Task where
    return x = Parallel x [] 
    x >>= f = joinTask (fmap f x)

instance Semigroup (Task a) where
    a <> b =
        case a of
            Parallel x y -> 
                Parallel x (b : y) 

f :: Monad m => Task (Thread a) -> (Thread a -> m (Progress (Thread a))) -> m (Task (Thread a))
f task tagger = 
    let helper :: Monad m => m (Task (Progress (Thread a))) -> m (Task (Thread a))
        helper = fmap (joinTask . fmap helper2)

        helper2 :: Progress (Thread a) -> Task (Thread a)
        helper2 progress =
            case progress of
                Done v -> pure v
                Step v -> pure v
                Blocked v -> pure v
                Branched a b -> pure a <> pure b


    in
    helper $ traverse tagger task

stepOneIf :: ReversibleLanguage a 
    => (Thread a -> Bool)
    -> Task (Thread a) 
    -> (Thread a -> Interpreter (Value a) (Progress (Thread a))) 
    -> Interpreter (Value a) (Progress (Task (Thread a)))
stepOneIf predicate (Parallel parent children) tagger = 
    let 
        rewrap parent (didWork, newChildren) = 
            if didWork then
                Step $ Parallel parent newChildren
            else 
                Done $ Parallel parent newChildren

        recurse parent = 
            foldrM (folder (\task -> stepOneIf predicate task tagger)) (False, []) children
                |> fmap (rewrap parent)
    
    in 
    if predicate parent then do
        newParent <- tagger parent

        case newParent of
            Step v -> 
                return $ Step $ Parallel v children

            Branched p c -> 
                return $ Step $ Parallel p (pure c : children) 

            Done v -> 
                recurse v
                    
            Blocked v -> 
                recurse v
    else
        recurse parent

throw :: Error -> StateT s (Either Error) a
throw error = State.StateT (\s -> Left error)


folder :: (ReversibleLanguage program) 
    => (Task (Thread program) -> Interpreter (Value program) (Progress (Task (Thread program))))
    -> Task (Thread program) 
    -> (Bool, List (Task (Thread program))) 
    -> Interpreter (Value program) (Bool, List (Task (Thread program)))
folder tagger current ( hasSucceeded, accum) =
    if hasSucceeded then 
        return ( True, current  : accum) 
    else do
        context <- State.get
        let result = runStateT (tagger current) context -- runStateT (forward current) context
        case result of
            Left e ->
                -- evaluating the child throws an error, try with another child
                return (False, current : accum)

            Right (updated, newContext) -> do
                put newContext
                case updated of
                    Step v -> 
                        return ( True, v : accum) 

                    Branched p c -> 
                        return ( True, (p <> c) : accum) 

                    Done v -> 
                        return ( False, v : accum) 

                    Blocked v -> 
                        return ( False, v : accum) 


{-| Will execute any (child) thread for which the predicate holds -} 
executeWhile :: ReversibleLanguage program => (Thread program -> Bool) -> Task (Thread program) -> Interpreter (Value program) (Task (Thread program))
executeWhile predicate task = do
    result <- stepOneIf predicate task forwardThread
    case result of
        Done v -> return v
        Step v -> execute v
        Blocked v -> return v
        Branched x y -> execute (x <> y) 

{-| Execute a program to completion (or deadlock) -} 
execute :: ReversibleLanguage program => Task (Thread program) -> Interpreter (Value program) (Task (Thread program))
execute task = do
    result <- stepOneIf (const True) task forwardThread
    case result of
        Done v -> return v
        Step v -> execute v
        Blocked v -> return v
        Branched x y -> execute (x <> y) 


unwrapProgress :: Progress (Task (Thread a)) -> Task (Thread a) 
unwrapProgress progress =
    case progress of
        Done v -> v
        Step v -> v
        Blocked v -> v
        Branched x y -> x <> y

type ExecutionState a = Either  (ThreadState a) (Thread a, ThreadState a)

{-| Evaluate a program one step forward -} 
forward :: ReversibleLanguage program => Thread program -> Interpreter (Value program) (ExecutionState program)
forward thread = 
    schedule (const True) thread (Map.empty, Map.empty, Map.empty, Map.empty)


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



