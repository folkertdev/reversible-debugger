{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (Thread(..), Task(..), Context, MonadInterpreter, forward, backward, rollVariable, rollThread, Interpreter.init, activeInactiveThreads) where

{-| The main body of code

The interesting stuff happens in the backward and forward functions.

-}

import Types
import Parser
import qualified Queue

import qualified Data.Map as Map 
import Data.Map (Map)
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State as State 
import Control.Monad.Trans (liftIO)
import Control.Applicative (Applicative, liftA2, pure, (<*))
import Control.Monad
import qualified Control.Exception as Exception (catch, BlockedIndefinitelyOnMVar)
import qualified System.Timeout
import qualified Text.ParserCombinators.Parsec as Parsec
import Data.Foldable (Foldable, foldrM)
import Control.Concurrent.Async as Async
import Data.Maybe (fromMaybe)
import Queue
import Data.Semigroup


data Context = 
    Context 
        { _threads :: Map ThreadName Int
        , variableCount :: Int
        , _bindings :: Map Identifier Value
        , _channels :: Map Identifier (Queue Identifier)
        }
        deriving (Show, Eq)


type MonadInterpreter a = State.StateT Context (Either Error) a

throw :: Error -> StateT s (Either Error) a
throw error = State.StateT (\s -> Left error)


data Task a 
    = Singleton (Thread a)
    | Parallel (Thread a) (Map ThreadName (Task a))
    deriving (Eq, Show)


data Thread a = Thread ThreadName (List History) (List a) deriving (Eq, Show)


withDefault :: a -> Maybe a -> a
withDefault = 
    fromMaybe 

init :: Program -> (Context,  Task Program)
init program = 
    ( Context (Map.singleton (ThreadName "t_0") 0) 0 Map.empty Map.empty
    , Singleton $ Thread (ThreadName "t_0") [] [ program ]
    )


listThreads :: Task a -> List (Thread a)
listThreads task = 
    case task of
        Parallel a b -> 
            a : concatMap listThreads b


        Singleton thread ->
            [ thread ] 

activeInactiveThreads :: Task a -> ( List ThreadName, List ThreadName ) 
activeInactiveThreads = 
    let 
        folder (Thread name _ program) ( active, inactive ) =
            case program of
                [] ->
                    ( active, name : inactive )

                _ -> 
                    ( name : active, inactive )
    in
        foldr folder ([], []) . listThreads  


{-| Generate a guaranteed unused (fresh) new identifier -}
freshIdentifier :: MonadInterpreter Identifier
freshIdentifier = do
    context <- State.get
    let new = 1 + variableCount context
    put (context { variableCount = new } )
    return $ Identifier $ "var" ++ show new


freshThreadName :: ThreadName -> MonadInterpreter ThreadName
freshThreadName parentName@(ThreadName parent) = do
    context <- State.get
    let usedThreadNames = _threads context  
    case Map.lookup parentName usedThreadNames of
        Nothing -> 
            throw $ RuntimeException "thread name without parent"

        Just childCount -> do
            let 
                childName = 
                    ThreadName $ parent ++ "_" ++ show (childCount + 1) 

                updater = 
                    Map.adjust (+ 1) parentName . Map.insert childName 0 

            State.put (context { _threads =  updater usedThreadNames }) 
            return childName


insertVariable :: Identifier -> Value -> MonadInterpreter () 
insertVariable identifier value = 
    State.modify $ \context -> 
        let 
            newBindings = Map.insert identifier value (_bindings context)
        in
            context { _bindings = newBindings } 


removeVariable :: Identifier -> MonadInterpreter ()  
removeVariable identifier = 
    State.modify $ \context -> 
        let 
            newBindings = Map.delete identifier (_bindings context)
        in
            context { _bindings = newBindings } 


{-| Get the value for an identifier from the global scope

throws an Error when the identifier is not defined
-} 
lookupVariable :: Identifier -> MonadInterpreter Value
lookupVariable identifier = do
    map <- _bindings <$> State.get
    case Map.lookup identifier map of
        Nothing ->
            throw (UndefinedVariable identifier)

        Just v ->
            return v

withChannel :: Identifier -> (Queue.Queue Identifier -> MonadInterpreter a) -> MonadInterpreter a
withChannel identifier tagger = do
    channel <- Map.lookup identifier . _channels <$> State.get 
    case channel of 
        Just queue -> 
            tagger queue

        Nothing ->
            throw $ UndefinedChannel identifier

mapChannel :: Identifier -> (Queue.Queue Identifier -> Queue.Queue Identifier) -> MonadInterpreter ()
mapChannel identifier tagger = 
    State.modify $ \context ->
        context { _channels = Map.adjust tagger identifier (_channels context) } 



readChannel :: ThreadName -> Identifier -> MonadInterpreter Identifier
readChannel threadName identifier = 
    withChannel identifier $ \queue ->
        case Queue.pop queue of
            Just ( first, rest ) -> do 
                -- put the rest of the queue back into the context
                mapChannel identifier (\_ -> rest)
                return first

            Nothing ->
                throw $ BlockedOnReceive threadName  


writeChannel :: ThreadName -> Identifier -> Identifier -> MonadInterpreter ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push payload)


lookupProcedure  :: Identifier -> MonadInterpreter (List Identifier, Program) 
lookupProcedure identifier = do
    value <- lookupVariable identifier
    case value of
        Procedure arguments body -> 
            return (arguments, body)

        _ ->
            throw $ TypeError identifier "I expected a Procedure but instead got" value


{-| Puts an either into an ExceptT context: Left throws an error, Right
continues the program
-} 
embedEither :: Monad m => Either e a  -> ExceptT e m a
embedEither v = ExceptT (return v)


insertChildTask :: Task a -> Map ThreadName (Task a) -> Map ThreadName (Task a)
insertChildTask task = 
    case task of
        Singleton (Thread name _ _) ->
            Map.insert name task 

        Parallel (Thread name _ _) _ -> 
            Map.insert name task


-- Move (Forward (Thread Program)) 

folder :: Task Program -> (Bool, Map ThreadName (Task Program)) -> MonadInterpreter (Bool, Map ThreadName (Task Program))
folder current ( hasSucceeded, accum) =
    if hasSucceeded then 
        return ( True, insertChildTask current accum) 
    else do
        context <- State.get
        let result = runStateT (forward current) context
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
tryForwardChildren :: Map ThreadName (Task Program) -> MonadInterpreter ( Bool, Map ThreadName (Task Program)) 
tryForwardChildren children = 
    foldrM folder (False, Map.empty) (Map.elems children)


handleBlockedOnReceive ::  Thread Program -> Map ThreadName (Task Program) -> Error -> MonadInterpreter (Task Program)
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


depthFirstEvaluate :: Map ThreadName (Task Program) -> Forward (Thread Program) -> MonadInterpreter (Task Program)
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
forward :: Task Program -> MonadInterpreter (Task Program)
forward task = 
    case task of 
        Parallel parent children -> 
            if Map.null children then
                fmap forwardToTask (forwardThread parent)

            else
                (depthFirstEvaluate children =<< forwardThread parent) `catch` handleBlockedOnReceive parent children

        Singleton (Thread name history []) -> 
            return task 

        Singleton thread@(Thread _ _ (_:_)) -> 
            fmap forwardToTask (forwardThread thread)


catch :: MonadInterpreter a -> (Error -> MonadInterpreter a) -> MonadInterpreter a
catch tryBlock handler = do
    context <- State.get
    case runStateT tryBlock context of
        Right ( value, newContext ) -> do
            put newContext
            return value

        Left error -> 
            handler error




forwardToTask :: Forward (Thread Program) -> Task Program
forwardToTask result = 
    case result of 
        Step updated ->
            Singleton updated

        Done  updated ->
            Singleton updated

        Branched newParent newChild ->
            Parallel newParent $ insertChildTask (Singleton newChild ) Map.empty

data Forward a  
    = Step a 
    | Branched a a
    | Done a 
    deriving (Show)


{-| Move a thread one step forward -} 
forwardThread :: Thread Program -> MonadInterpreter (Forward (Thread Program)) 
forwardThread thread = 
    case thread of 
        Thread _ _ [] -> 
            -- the thread contains no further instructions
            return $ Done thread

        Thread name history (program : rest) -> 
            let 
                continue :: History -> List Program -> MonadInterpreter (Forward (Thread Program)) 
                continue historyInstruction instructions = 
                    return $ Step (Thread name (historyInstruction : history) instructions)
            in
            case program of
                Skip ->
                    continue Skipped rest 

                Sequence a b ->
                    -- add Esc markers to retrieve the branches when reversing
                    continue Composed (a:Esc:b:Esc:rest)

                Let identifier value continuation -> do
                    freshName <- freshIdentifier 
                    (historyInstruction, newName) <- 
                        case value of
                            Receive channelName -> do 
                                message <- readChannel name channelName 
                                return (Received channelName freshName, message)

                            Port -> do
                                State.modify $ \context -> 
                                    let 
                                        newChannels = Map.insert freshName Queue.empty (_channels context)
                                    in
                                        context { _channels = newChannels } 

                                return (CreatedChannel freshName, freshName)

                            Procedure arguments body -> do
                                -- rename the function name itself in the body, for recursive functions
                                let renamedBody = 
                                        if identifier `notElem` arguments then
                                            renameVariable identifier freshName body
                                        else
                                            body

                                insertVariable freshName (Procedure arguments renamedBody) 
                                return (CreatedVariable freshName, freshName)
                                

                            _ -> do
                                insertVariable freshName value 
                                return (CreatedVariable freshName, freshName)

                    continue historyInstruction $
                        renameVariable identifier newName continuation : Esc : rest

                If condition trueBody falseBody -> do
                    verdict <- evalBoolExp condition 
                    if verdict then
                        continue (BranchedOn condition True falseBody) (trueBody : Esc : rest)
                    else
                        continue (BranchedOn condition False trueBody) (falseBody : Esc : rest)

                Assert condition -> do
                    verdict <- evalBoolExp condition 
                    if not verdict then
                        throw $ AssertionError condition
                    else
                        continue (AssertedOn condition) rest


                SpawnThread work -> do
                    threadName <- freshThreadName name
                    return $ Branched 
                        ( Thread name (SpawnedThread threadName : history) rest)
                        ( Thread threadName [] [ work ])

                Apply functionName arguments -> do
                    ( parameters, body ) <- lookupProcedure functionName 
                    if length arguments /= length parameters then
                        throw $ ArgumentMismatch functionName (length parameters) (length arguments) 
                    else 
                        let
                            withRenamedVariables = 
                                    foldr (uncurry renameVariable) body (zip parameters arguments)
                        in
                            continue (CalledProcedure functionName arguments) (withRenamedVariables : Esc : rest)
                                
                Send channelName variable -> do
                    writeChannel name channelName variable 
                    continue (Sent channelName) rest

                Esc ->
                    continue HistoryEsc rest


-- Move Backward

backward :: Task Program -> MonadInterpreter (Task Program)
backward task = 
    case task of
        Singleton thread ->
            Singleton <$> backwardThread thread            

        Parallel parent children ->
            case parent of
                Thread parentName (SpawnedThread spawnedName : restOfHistory) program -> 
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
                                    return $ Singleton $ Thread parentName restOfHistory $ SpawnThread x : program 

                                _ ->
                                    error "invalid initial thread state" 

                            

                        Just task -> do
                            updatedChild <- backward task
                            return $ Parallel parent (Map.adjust (\_ -> updatedChild) spawnedName children)

                _ -> 
                    -- reverse the parent thread, keeping the children constant
                    -- liftIO $ print "parallel is not a spawn, so rolling parent"
                    -- liftIO $ print task
                    Parallel <$> backwardThread parent <*> pure children


{-| Move a thread one step backward -} 
backwardThread :: Thread Program -> MonadInterpreter (Thread Program)
backwardThread thread@(Thread name history program) =
    case history of
        [] -> 
            -- do nothing
            return thread
        
        ( mostRecent : restOfHistory ) ->
            let 
                continue :: List Program -> MonadInterpreter (Thread Program)
                continue = return . Thread name restOfHistory 
            in
            case (mostRecent, program) of
                ( Skipped, restOfProgram ) ->
                    continue (Skip : program)

                ( Composed, first : Esc : second : Esc : restOfProgram ) ->
                    continue (Sequence first second : restOfProgram)

                ( CreatedVariable identifier, continuation : Esc : restOfProgram ) -> do
                    value <- lookupVariable identifier 
                    removeVariable identifier 
                    continue (Let identifier value continuation : restOfProgram )

                ( CreatedChannel identifier, continuation : Esc : restOfProgram ) -> do
                    State.modify $ \context -> 
                        context { _channels = Map.delete identifier (_channels context) } 

                    continue (Let identifier Port continuation : restOfProgram )

                ( BranchedOn condition True falseBody, trueBody : Esc : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)
                    
                ( BranchedOn condition False trueBody, falseBody : Esc : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)

                ( AssertedOn condition, restOfProgram ) ->
                    continue (Assert condition : restOfProgram)

                ( CalledProcedure functionName arguments, body : Esc : restOfProgram ) ->
                    continue (Apply functionName arguments : restOfProgram)

                ( Sent channelName, restOfProgram ) -> do
                    -- reverse of send is receive
                    message <- readChannel name channelName 

                    continue $ Send channelName message : restOfProgram

                ( Received channelName valueName, continuation : Esc : restOfProgram ) -> do
                    -- reverse of receive is send
                    writeChannel name channelName valueName 

                    -- unbind the variable
                    removeVariable valueName 

                    continue $ Let valueName (Receive channelName) continuation : restOfProgram 

                ( HistoryEsc, restOfProgram ) ->
                    continue $ Esc : restOfProgram
                    
                ( _, restOfProgram) ->
                    -- the program has a pattern incompatible with the history instruction we're currently matching
                    error $ show mostRecent ++ " encountered a pattern it cannot match: " ++ show restOfProgram 


-- Rolls 

{-| Revert the program state before the creation of the given variable -}
rollVariable :: Identifier -> Task Program -> MonadInterpreter (Task Program)
rollVariable name task = do 
    lookupVariable name -- will throw if the name does not exist
    case task of
        Singleton (Thread _ (CreatedVariable identifier : restOfHistory) program) ->
            if name == identifier then  
                backward task

            else
                rollVariable name =<< backward task

        _ ->
            rollVariable name =<< backward task


{-| Revert a whole thread -} 
rollThread :: ThreadName -> Task Program -> MonadInterpreter (Task Program)
rollThread threadName task = do
    let recurse task = rollThread threadName =<< backward task 

    exists <- Map.member threadName . _threads <$> State.get
    if not exists then
        throw $ UndefinedThread threadName
    else
        case task of
            Parallel parent child ->
                recurse task 

            Singleton (Thread _ [] _) ->
                return task

            Singleton (Thread parentName (SpawnedThread spawnedName : restOfHistory) parentProgram) -> 
                if spawnedName == threadName then
                    -- undo the spawning, the rest is undone
                    backward task

                else
                    recurse task

            Singleton (Thread name history program) ->
                if name == threadName then
                    -- this is the thread we want to unroll
                    recurse task
                else
                    return task


-- Helpers 
                        

evalBoolExp :: BoolExp -> MonadInterpreter Bool 
evalBoolExp expression = 
    let
        toFunction operator =
            case operator of
                Equal -> 
                    (==)

                LessThan -> 
                    (<)

                GreaterThan -> 
                    (>)

                LessThanEqual -> 
                    (<=)

                GreaterThanEqual  -> 
                    (>=)
    in
        case expression of 
            AtomBool bool -> 
                evalBoolValue bool

            Operator op a b -> 
                liftA2 (toFunction op) (evalIntValue a) (evalIntValue b)


evalIntExp :: IntExp -> MonadInterpreter Int 
evalIntExp expression = 
    let evalOperator f a b = do
            x <- evalIntValue  a
            y <- evalIntExp  b
            return $ f x y
    in
        case expression of 
            AtomInt int -> 
                evalIntValue int 

            Add a b ->
                evalOperator (+) a b

            Subtract a b ->
                evalOperator (-) a b

            Multiply a b ->
                evalOperator (*) a b

            Divide a b ->
                evalOperator div a b


evalIntValue :: IntValue -> MonadInterpreter Int
evalIntValue value =
    case value of
        IntValue int -> 
            return int

        IntIdentifier reference -> do
            dereferenced <- lookupVariable reference 
            case dereferenced of
                VInt intExpr ->
                    evalIntExp intExpr

                _ ->
                    throw $ TypeError reference "I expected an IntExpr but got" dereferenced

             
evalBoolValue :: BoolValue -> MonadInterpreter Bool
evalBoolValue value = 
    case value of
        BoolValue bool -> 
            return bool

        BoolIdentifier identifier -> do
            value <- lookupVariable identifier 
            case value of 
                VTrue -> 
                    return True

                VFalse ->
                    return False

                other -> 
                    throw $ TypeError identifier "I expected a boolean value but got" other
