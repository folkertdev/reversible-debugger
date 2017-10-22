{-# LANGUAGE ScopedTypeVariables, TypeFamilies, AllowAmbiguousTypes, Rank2Types, FlexibleContexts, StandaloneDeriving, UndecidableInstances  #-}
module ReversibleLanguage (Context(..), Interpreter, Progress(..), Task(..), Thread(..), ReversibleLanguage(..), forward, backward, throw) where

import Types
import Queue
import Data.Map as Map
import Data.Map (Map)
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State as State 
import Control.Applicative (Applicative, liftA2, pure, (<*))
import Control.Monad
import Data.Foldable (Foldable, foldrM)
import Data.Maybe (fromMaybe)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

data Context a = 
    Context 
        { _threads :: Map ThreadName Int
        , variableCount :: Int
        , _bindings :: Map Identifier a
        , _channels :: Map Identifier (Queue.Queue Identifier)
        }
    deriving (Show)


type Interpreter value program = State.StateT (Context value) (Either Error) program


class (Eq a, Show a, Show (Value a), Show (History a), Eq (Value a), Eq (History a)) => ReversibleLanguage a where 
    data Value a :: * 
    data History a :: * 
    forwardThread :: Thread a -> Interpreter (Value a) (Progress (Thread a))
    backwardThread :: Thread a -> Interpreter (Value a) (Progress (Thread a))

    spawn :: a -> a

    spawned :: History a -> Maybe ThreadName
    createdVariable :: History a -> Maybe Identifier


data Progress a  
    = Step a 
    | Branched a a
    | Done a 
    deriving (Show, Eq)

data Task a 
    = Singleton (Thread a)
    | Parallel (Thread a) (Map ThreadName (Task a))
    

data Thread a = Thread ThreadName (List (History a)) (List a)  

deriving instance (Eq a, Eq (History a)) => Eq (Thread a) 
deriving instance (Show a, Show (History a)) => Show (Thread a) 

deriving instance Show (Thread a) => Show (Task a)
deriving instance Eq (Thread a) => Eq (Task a)





throw :: Error -> StateT s (Either Error) a
throw error = State.StateT (\s -> Left error)

-- Move (Forward (Thread Program)) 
insertChildTask :: Task a -> Map ThreadName (Task a) -> Map ThreadName (Task a)
insertChildTask task = 
    case task of
        Singleton (Thread name _ _) ->
            Map.insert name task 

        Parallel (Thread name _ _) _ -> 
            Map.insert name task


folder :: (ReversibleLanguage program) => Task program -> (Bool, Map ThreadName (Task program)) -> Interpreter (Value program) (Bool, Map ThreadName (Task program))
folder current ( hasSucceeded, accum) =
    if hasSucceeded then 
        return ( True, insertChildTask current accum) 
    else do
        context <- State.get
        let result = runStateT (forward current) context -- runStateT (forward current) context
        case result of
            Left e ->
                -- evaluating the child throws an error, try with another child
                return (False, insertChildTask current accum)

            Right (updated, newContext) -> do
                put newContext
                if updated /= current then 
                    -- we've made one step of progres, that's enough
                    return (True, insertChildTask updated accum)

                else
                    return (False, insertChildTask current accum)


{-| Tries to forward a child. When one child has made progress, the rest is not evaluated further
-}
tryForwardChildren :: ReversibleLanguage program => Map ThreadName (Task program) -> Interpreter (Value program) ( Bool, Map ThreadName (Task program)) 
tryForwardChildren children = 
    foldrM folder (False, Map.empty) (Map.elems children)


handleBlockedOnReceive :: ReversibleLanguage program => Thread program -> Map ThreadName (Task program) -> Error -> Interpreter (Value program) (Task program)
handleBlockedOnReceive parent children e =
    -- the parent is blocked on a receive. Let's try whether its children can make progress
    -- thereby hopefully fixing the blocking
    case e of
        BlockedOnReceive _ -> do
            (progress, newChildren) <- tryForwardChildren children
            if not progress then
                -- none of the children can make progress so throw the original error
                throw e 

            else
                return $ Parallel parent newChildren

        _ -> 
            throw e


depthFirstEvaluate :: ReversibleLanguage program => Map ThreadName (Task program) -> Progress (Thread program) -> Interpreter (Value program) (Task program)
depthFirstEvaluate children result = 
    case result of
        Done newParent -> do 
            -- if the parent is done, try to make
            -- progress in the children
            (progress, newChildren) <- tryForwardChildren children
            return $ Parallel newParent newChildren

        Step newParent -> 
            -- the parent made progress, don't look at the children
            return $ Parallel newParent children

        Branched a b -> 
            -- the parent made progress, don't look at the children
            return $ Parallel a (insertChildTask (Singleton b) children) 


{-| Evaluate a program one step forward -} 
forward :: ReversibleLanguage program => Task program -> Interpreter (Value program) (Task program)
forward task = 
    case task of 
        Parallel parent children -> 
            if Map.null children then
                fmap progressToTask (forwardThread parent)

            else
                (depthFirstEvaluate children =<< forwardThread parent) `catch` handleBlockedOnReceive parent children

        Singleton (Thread name history []) -> 
            return task 

        Singleton thread@(Thread _ _ (_:_)) -> 
            fmap progressToTask (forwardThread thread)


catch :: Interpreter v a -> (Error -> Interpreter v a) -> Interpreter v a
catch tryBlock handler = do
    context <- State.get
    case runStateT tryBlock context of
        Right ( value, newContext ) -> do
            put newContext
            return value

        Left error -> 
            handler error




progressToTask :: Progress (Thread program) -> Task program
progressToTask result = 
    case result of 
        Step updated ->
            Singleton updated

        Done  updated ->
            Singleton updated

        Branched newParent newChild ->
            Parallel newParent $ insertChildTask (Singleton newChild ) Map.empty

-- Move Backward

backward :: ReversibleLanguage program => Task program -> Interpreter (Value program) (Task program)
backward task = 
    case task of
        Singleton thread ->
            progressToTask <$> backwardThread thread            

        Parallel parent children ->
            case parent of 
                Thread parentName [] program ->
                    if Map.null children then
                         return $ Singleton parent
                    else
                        error "parent is done but there are still children alive"

                Thread parentName (mostRecent : restOfHistory) program ->
                    case spawned mostRecent of
                        Nothing -> do
                            -- reverse the parent thread, keeping the children constant
                            newParent <- progressToTask <$> backwardThread parent 
                            return $ 
                                case newParent of 
                                    Parallel p t -> 
                                        -- Parallel p (Map.union t children)
                                        error "backwards action produced a thread."

                                    Singleton p -> 
                                        Parallel p children

                        Just spawnedName -> 
                            -- first empty the history of the child before reverting the parent further
                            case Map.lookup spawnedName children of
                                Nothing -> 
                                    error "non-existent child spawned"

                                Just (Singleton (Thread childName [] threadBody)) -> do
                                    -- child is already completely rolled 
                                    State.modify $ \context -> 
                                        let 
                                            newThreads = Map.adjust (\v -> v - 1 :: Int) parentName $ Map.delete childName (_threads context)
                                        in
                                            context { _threads = newThreads } 

                                    case threadBody of
                                        [x] -> 
                                            return $ Singleton $ Thread parentName restOfHistory $ spawn x : program 

                                        _ ->
                                            error "invalid initial thread state" 

                                    

                                Just task -> do
                                    updatedChild <- backward task
                                    return $ Parallel parent (Map.adjust (const updatedChild) spawnedName children)



                        

