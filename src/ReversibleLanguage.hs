 {-# LANGUAGE ScopedTypeVariables , TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module ReversibleLanguage (Context(..), Interpreter, Progress(..), Task(..), Thread(..), ReversibleLanguage(..), forward, backward, throw, executeWhile, catch) where

import Types
import Queue
import Data.Semigroup (Semigroup(..)) 
import Data.Map (Map)
import qualified Data.Map as Map
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


type PID = List Int

type Threads a = Map PID (Thread a)


data Context a = 
    Context 
        { _threads :: Map ThreadName Int
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

    spawned :: History a -> Maybe ThreadName
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
    

data Thread a = Thread ThreadName (List (History a)) (List a)  

{-
spawnThread :: PID -> a -> Threads a -> Threads a
spawnThread parentID value threads = 
    let 
        siblings = Map.keys threads 
            |> filter (\k -> length k == length parentID + 1 && parentID `isPrefixOf` k)
        childID = parentID ++ [ length siblings ]
    in
        Map.insert childID (Thread (ThreadName $ show childID) [] value) threads
-}

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


{-| Evaluate a program one step forward -} 
forward :: ReversibleLanguage program => Task (Thread program) -> Interpreter (Value program) (Task (Thread program))
forward task = 
    unwrapProgress <$> stepOneIf (const True) task forwardThread




catch :: Interpreter v a -> (Error -> Interpreter v a) -> Interpreter v a
catch tryBlock handler = do
    context <- State.get
    case runStateT tryBlock context of
        Right ( value, newContext ) -> do
            put newContext
            return value

        Left error -> 
            handler error


-- Move Backward

backward :: ReversibleLanguage program => Task (Thread program) -> Interpreter (Value program) (Task (Thread program))
backward (Parallel parent children) = 
    case parent of 
        Thread parentName [] program ->
            if Prelude.null children then
                 return $ Parallel parent []
            else
                error "parent is done but there are still children alive"

        Thread parentName (mostRecent : restOfHistory) program ->
            case spawned mostRecent of
                Nothing -> do
                    -- reverse the parent thread, keeping the children constant
                    newParent <- backwardThread parent 
                    case newParent of
                        Done v -> return $ Parallel v children
                        Step v -> return $ Parallel v children
                        Branched x y -> error "step backward branched" 
                        Blocked v -> return $ Parallel v children


                Just spawnedName -> 
                    -- first empty the history of the child before reverting the parent further
                    case find (\(Parallel (Thread name _ _) _) -> name == spawnedName) children of
                        Nothing -> 
                            error "non-existent child spawned"

                        Just currentChild@(Parallel (Thread childName [] threadBody) []) -> do
                            -- child is already completely rolled 
                            State.modify $ \context -> 
                                let 
                                    newThreads = Map.adjust (\v -> v - 1 :: Int) parentName $ Map.delete childName (_threads context)
                                in
                                    context { _threads = newThreads } 

                            case threadBody of
                                [x] -> 
                                    return $ Parallel (Thread parentName restOfHistory $ spawn x : program) (Prelude.filter (/= currentChild) children)

                                _ ->
                                    error "invalid initial thread state" 

                            

                        Just task -> do
                            updatedChild <- backward task
                            return $ Parallel parent $ Prelude.map (\child ->
                                if child == task then
                                    updatedChild 
                                else 
                                    child 
                                                                   ) children



                

