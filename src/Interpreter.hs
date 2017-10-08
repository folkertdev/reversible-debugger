{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (Thread(..), Task(..),forward, backward, rollVariable, rollThread, Interpreter.init, activeInactiveThreads) where

{-| The main body of code

The interesting stuff happens in the backward and forward functions.

-}

import Types
import Parser

import qualified Data.Map as Map 
import Data.Map (Map)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, myThreadId, ThreadId, killThread)
import Data.IORef
import Control.Concurrent.MVar 
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans (liftIO)
import Control.Applicative (Applicative, liftA2, pure, (<*))
import Control.Monad
import qualified Control.Exception as Exception (catch, BlockedIndefinitelyOnMVar)
import qualified System.Timeout
import Text.ParserCombinators.Parsec as Parsec
import Data.Foldable (Foldable, foldrM)
import Control.Concurrent.Async as Async
import Data.Maybe (fromMaybe)


data Task a 
    = Singleton (Thread a)
    | Parallel (Thread a) (Map ThreadName (Task a))
    deriving (Eq, Show)


data Thread a = Thread ThreadName (List History) (List a) deriving (Eq, Show)


withDefault :: a -> Maybe a -> a
withDefault = 
    fromMaybe 

init :: Program -> IO (MVar Int, MVar (Map.Map Identifier Value), MVar (Map ThreadName Int),  Task Program)
init program = do
    bindings <- newMVar Map.empty
    identifierRef <- newMVar 0
    threadNameRef <- newMVar $ Map.singleton (ThreadName "t_0") 0

    return  
        ( identifierRef
        , bindings
        , threadNameRef
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
freshIdentifier :: MVar Int -> IO Identifier  
freshIdentifier ref = do
    new <- modifyMVar ref (\v -> return (v + 1, v + 1))
    return $ Identifier $ "var" ++ show new

freshThreadName :: MVar (Map.Map ThreadName Int) -> ThreadName -> IO ThreadName
freshThreadName usedThreadNamesRef parentName@(ThreadName parent) = do
    usedThreadNames <- readMVar usedThreadNamesRef
    case Map.lookup parentName usedThreadNames of
        Nothing -> 
            error "thread name without parent"

        Just childCount -> do
            let 
                childName = 
                    ThreadName $ parent ++ "_" ++ show (childCount + 1) 

                updater = 
                    Map.adjust (+ 1) parentName . Map.insert childName 0 

            modifyMVar_ usedThreadNamesRef (return . updater) 
            return childName
            

insertVariable :: Identifier -> Value -> MVar (Map.Map Identifier Value) -> IOThrowsError ()
insertVariable identifier value ref = 
    liftIO $ modifyMVar_ ref (return . Map.insert identifier value)


removeVariable :: Identifier -> MVar (Map.Map Identifier Value) -> IOThrowsError ()
removeVariable identifier ref = 
    liftIO $ modifyMVar_ ref (return . Map.delete identifier)


{-| Get the value for an identifier from the global scope

throws an Error when the identifier is not defined
-} 
lookupVariable :: Identifier -> MVar (Map.Map Identifier Value) -> IOThrowsError Value
lookupVariable identifier ref = do
    map <- liftIO $ readMVar ref
    case Map.lookup identifier map of
        Nothing ->
            throwE (UndefinedVariable identifier)

        Just v ->
            return v


lookupChannel  :: Identifier -> MVar (Map.Map Identifier Value) -> IOThrowsError (Chan Identifier)
lookupChannel identifier ref = do
    value <- lookupVariable identifier ref
    case value of
        Port (Just channel) ->
            return channel

        Port Nothing ->
            throwE $ UninitializedChannel identifier

        _ ->
            throwE $ TypeError identifier "I expected a Port but instead got" value


lookupProcedure  :: Identifier -> MVar (Map.Map Identifier Value) -> IOThrowsError (List Identifier, Program) 
lookupProcedure identifier ref = do
    value <- lookupVariable identifier ref
    case value of
        Procedure arguments body -> 
            return (arguments, body)

        _ ->
            throwE $ TypeError identifier "I expected a Procedure but instead got" value


{-| Puts an either into an ExceptT context: Left throws an error, Right
continues the program
-} 
embedEither :: Monad m => Either e a  -> ExceptT e m a
embedEither v = ExceptT (return v)


readChannelWithTimeout :: ThreadName ->  Chan a -> Int -> IOThrowsError a
readChannelWithTimeout thread channel duration = 
    let 
        body :: Exception.BlockedIndefinitelyOnMVar -> IO (Either Error a)
        body _ = return $ Left $ BlockedOnReceive thread 

        withTimeout = do
            result <- System.Timeout.timeout duration (readChan channel)
            case result of
                Just r ->
                    return $ Right r

                Nothing ->
                    return $ Left $ BlockedOnReceive thread 
    in do
        result <- liftIO $ Exception.catch withTimeout body 
        embedEither result


insertChildTask :: Task a -> Map ThreadName (Task a) -> Map ThreadName (Task a)
insertChildTask task = 
    case task of
        Singleton (Thread name _ _) ->
            Map.insert name task 

        Parallel (Thread name _ _) _ -> 
            Map.insert name task


folder :: (Task Program -> IOThrowsError (Task Program)) -> (Bool, Map ThreadName (Task Program)) -> Task Program -> IOThrowsError (Bool, Map ThreadName (Task Program))
folder run ( hasSucceeded, accum) current =
    if hasSucceeded then 
        return ( True, insertChildTask current accum) 
    else do
        result <- liftIO $ runExceptT $ run current
        case result of
            Left e ->
                -- evaluating the child throws an error, try with another child
                return (False, insertChildTask current accum)

            Right updated -> 
                if updated /= current then 
                    -- we've made one step of progres, that's enough
                    return (True, insertChildTask updated accum)

                else
                    return (False, insertChildTask current accum)


{-| Tries to forward a child. When one child has made progress, the rest is not evaluated further
-}
tryForwardChildren :: (Task Program -> IOThrowsError (Task Program)) -> Map ThreadName (Task Program) -> IOThrowsError ( Bool, Map ThreadName (Task Program)) 
tryForwardChildren run children = 
    foldrM (flip $ folder run) (False, Map.empty) (Map.elems children)


handleBlockedOnReceive :: (Task Program -> IOThrowsError (Task Program)) -> Thread Program -> Map ThreadName (Task Program) -> Error -> IOThrowsError (Task Program)
handleBlockedOnReceive run parent children e =
    case e of
        BlockedOnReceive _ -> do
            (progress, newChildren) <- tryForwardChildren run children
            if not progress then
                -- none of the children can make progress so throw the original error
                throwE e 

            else
                return $ Parallel parent newChildren

        _ -> 
            throwE e


depthFirstEvaluate :: (Task Program -> IOThrowsError (Task Program)) -> Map ThreadName (Task Program) -> Forward -> IOThrowsError (Task Program)
depthFirstEvaluate run children result = 
    case result of
        Done newParent -> do 
            -- if the parent is done, try to make
            -- progress in the children
            (progress, newChildren) <- tryForwardChildren run children
            return $ Parallel newParent newChildren

        Step newParent -> 
            return $ Parallel newParent children

        Branched a b -> 
            return $ Parallel a (insertChildTask (Singleton b) children) 


{-| Evaluate a program one step forward -} 
forward :: MVar Int -> MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Task Program -> IOThrowsError (Task Program)
forward identifierRef bindings threadNames task = 
    let 
        fthread = forwardThread identifierRef bindings threadNames 
    in
    case task of 
        Parallel parent children -> 
            if Map.null children then
                fmap forwardToTask (fthread parent)

            else
                let 
                    run = forward identifierRef bindings threadNames
                in 
                    (depthFirstEvaluate run children =<< fthread parent) `catchE` handleBlockedOnReceive run parent children

        Singleton (Thread name history []) -> 
            return task 

        Singleton thread@(Thread _ _ (_:_)) -> 
            fmap forwardToTask (fthread thread)


forwardToTask :: Forward -> Task Program
forwardToTask result = 
    case result of 
        Step updated ->
            Singleton updated

        Done  updated ->
            Singleton updated

        Branched newParent newChild ->
            Parallel newParent $ insertChildTask (Singleton newChild ) Map.empty

data Forward 
    = Step (Thread Program) 
    | Branched (Thread Program) (Thread Program) 
    | Done (Thread Program) 
    deriving (Show)

{-| Move a thread one step forward -} 
forwardThread :: MVar Int -> MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Thread Program -> IOThrowsError Forward 
forwardThread identifierRef bindings threadNames thread@(Thread name history []) = return (Done thread) 
forwardThread identifierRef bindings threadNames (Thread name history (program : rest)) = 
            let 
                continue :: History -> List Program -> IOThrowsError Forward 
                continue historyInstruction instructions = 
                    return $ Step (Thread name (historyInstruction : history) instructions)
            in
            case program of
                Skip ->
                    continue Skipped rest 

                Sequence a b ->
                    continue Composed (a:Esc:b:Esc:rest)

                Let identifier value continuation -> do
                    freshName <- liftIO $ freshIdentifier identifierRef
                    (historyInstruction, newName) <- 
                        case value of
                            Receive channelName -> do 
                                channel <- lookupChannel channelName bindings
                                message <- readChannelWithTimeout name channel 200 -- liftIO $ readChan channel
                                return (Received channelName freshName, message)

                            Port Nothing -> do 
                                channel <- liftIO newChan
                                insertVariable freshName (Port (Just channel)) bindings
                                return (CreatedVariable freshName, freshName)

                            Procedure arguments body -> do
                                -- rename the function name itself in the
                                -- body, for recursive functions
                                let renamedBody = 
                                        if identifier `notElem` arguments then
                                            renameVariable identifier freshName body
                                        else
                                            body

                                insertVariable freshName (Procedure arguments renamedBody) bindings
                                return (CreatedVariable freshName, freshName)
                                

                            _ -> do
                                insertVariable freshName value bindings
                                return (CreatedVariable freshName, freshName)

                    continue historyInstruction $
                        renameVariable identifier newName continuation : Esc : rest

                If condition trueBody falseBody -> do
                    verdict <- evalBoolExp bindings condition 
                    if verdict then
                        continue (BranchedOn condition True falseBody) (trueBody : Esc : rest)
                    else
                        continue (BranchedOn condition False trueBody) (falseBody : Esc : rest)

                Assert condition -> do
                    verdict <- evalBoolExp bindings condition 
                    if not verdict then
                        throwE $ AssertionError condition
                    else
                        continue (AssertedOn condition) rest


                SpawnThread work -> do
                    threadName <- liftIO $ freshThreadName threadNames name
                    return $ Branched 
                        ( Thread name (SpawnedThread threadName : history) rest)
                        ( Thread threadName [] [ work ])

                Apply functionName arguments -> do
                    ( parameters, body ) <- lookupProcedure functionName bindings

                    let folder (from, to) = 
                            renameVariable from to 

                        withRenamedVariables = 
                            foldr folder body (zip parameters arguments)

                    continue (CalledProcedure functionName arguments) (withRenamedVariables : Esc : rest)
                                
                Send channelName variable -> do
                    channel <- lookupChannel channelName bindings

                    liftIO $ writeChan channel variable

                    continue (Sent channelName) rest

                Esc ->
                    continue HistoryEsc rest


{-| Revert the program state before the creation of the given variable -}
rollVariable :: Identifier -> MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Task Program -> IOThrowsError (Task Program)
rollVariable name bindings threadNames task = do 
    lookupVariable name bindings -- will throw if the name does not exist
    case task of
        Singleton (Thread _ (CreatedVariable identifier : restOfHistory) program) ->
            if name == identifier then  
                backward bindings threadNames task

            else
                rollVariable name bindings threadNames =<< backward bindings threadNames task

        _ ->
            rollVariable name bindings threadNames =<< backward bindings threadNames task


{-| Revert a whole thread -} 
rollThread :: ThreadName -> MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Task Program -> IOThrowsError (Task Program)
rollThread threadName bindings threadNames task = 
    let 
        recurse task = 
            rollThread threadName bindings threadNames =<< backward bindings threadNames task


    in do

    exists <- Map.member threadName <$> liftIO (readMVar threadNames)
    if not exists then
        throwE $ UndefinedThread threadName
    else
        case task of
            Parallel parent child ->
                recurse task 

            Singleton (Thread _ [] _) ->
                return task

            Singleton (Thread parentName (SpawnedThread spawnedName : restOfHistory) parentProgram) -> 
                if spawnedName == threadName then
                    -- undo the spawning, the rest is undone
                    backward bindings threadNames task

                else
                    recurse task

            Singleton (Thread name history program) ->
                if name == threadName then
                    -- this is the thread we want to unroll
                    recurse task
                else
                    return task
        

backward :: MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Task Program -> IOThrowsError (Task Program)
backward bindings threadNames task = 
    case task of
        Singleton thread ->
            Singleton <$> backwardThread bindings threadNames thread            

        Parallel parent children ->
            case parent of
                Thread parentName (SpawnedThread spawnedName : restOfHistory) program -> 
                    -- first empty the history of the child before reverting the parent further
                    case Map.lookup spawnedName children of
                        Nothing -> 
                            error "non-existent child spawned"

                        Just (Singleton (Thread childName [] threadBody)) -> do
                            -- child is already completely rolled 
                            let 
                                updater = 
                                    Map.adjust (\v -> v - 1 :: Int) parentName . Map.delete childName

                            liftIO $ modifyMVar_ threadNames (return . updater)  
                            case threadBody of
                                [x] -> 
                                    return $ Singleton $ Thread parentName restOfHistory $ SpawnThread x : program 

                                _ ->
                                    error "invalid initial thread state" 

                            

                        Just task -> do
                            updatedChild <- backward bindings threadNames task
                            return $ Parallel parent (Map.adjust (\_ -> updatedChild) spawnedName children)

                _ -> do
                    -- reverse the parent thread, keeping the children constant
                    liftIO $ print "parallel is not a spawn, so rolling parent"
                    liftIO $ print task
                    Parallel <$> backwardThread bindings threadNames parent <*> pure children


{-| Move a thread one step backward -} 
backwardThread :: MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Thread Program -> IOThrowsError (Thread Program)
backwardThread bindings threadNames thread@(Thread name history program) =
    case history of
        [] -> 
            -- do nothing
            return thread
        
        ( mostRecent : restOfHistory ) ->
            let 
                continue :: List Program -> IOThrowsError (Thread Program)
                continue = return . Thread name restOfHistory 
            in
            case (mostRecent, program) of
                ( Skipped, restOfProgram ) ->
                    continue (Skip : program)

                ( Composed, first : Esc : second : Esc : restOfProgram ) ->
                    continue (Sequence first second : restOfProgram)

                ( CreatedVariable identifier, continuation : Esc : restOfProgram ) -> do
                    value <- lookupVariable identifier bindings
                    removeVariable identifier bindings
                    continue (Let identifier value continuation : restOfProgram )

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
                    channel <- lookupChannel channelName bindings
                    message <- readChannelWithTimeout name channel 200 

                    continue $ Send channelName message : restOfProgram

                ( Received channelName valueName, continuation : Esc : restOfProgram ) -> do
                    -- reverse of receive is send
                    channel <- lookupChannel channelName bindings
                    liftIO $ writeChan channel valueName

                    -- unbind the variable
                    removeVariable valueName bindings

                    continue $ Let valueName (Receive channelName) continuation : restOfProgram 

                ( HistoryEsc, restOfProgram ) ->
                    continue $ Esc : restOfProgram
                    
                ( _, restOfProgram) ->
                    -- the program has a pattern incompatible with the history instruction we're currently matching
                    error $ show mostRecent ++ " encountered a pattern it cannot match: " ++ show restOfProgram 


-- Helpers 
                        

evalBoolExp :: MVar (Map.Map Identifier Value) -> BoolExp -> IOThrowsError Bool
evalBoolExp environment expression = 
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
                evalBoolValue environment bool

            Operator op a b -> 
                liftA2 (toFunction op) (evalIntValue environment a) (evalIntValue environment b)


evalIntExp :: MVar (Map.Map Identifier Value) -> IntExp -> IOThrowsError Int
evalIntExp environment expression = 
    let evalOperator f a b = do
            x <- evalIntValue environment a
            y <- evalIntExp environment b
            return $ f x y
    in
        case expression of 
            AtomInt int -> 
                evalIntValue environment int 

            Add a b ->
                evalOperator (+) a b

            Subtract a b ->
                evalOperator (-) a b

            Multiply a b ->
                evalOperator (*) a b

            Divide a b ->
                evalOperator div a b


evalIntValue :: MVar (Map.Map Identifier Value) -> IntValue -> IOThrowsError Int
evalIntValue bindings value =
    case value of
        IntValue int -> 
            return int

        IntIdentifier reference -> do
            dereferenced <- lookupVariable reference bindings 
            case dereferenced of
                VInt intExpr ->
                    evalIntExp bindings intExpr

                _ ->
                    throwE $ TypeError reference "I expected an IntExpr but got" dereferenced

             
evalBoolValue :: MVar (Map.Map Identifier Value) -> BoolValue -> IOThrowsError Bool
evalBoolValue bindings value = 
    case value of
        BoolValue bool -> 
            return bool

        BoolIdentifier identifier -> do
            value <- lookupVariable identifier bindings
            case value of 
                VTrue -> 
                    return True

                VFalse ->
                    return False

                other -> 
                    throwE $ TypeError identifier "I expected a boolean value but got" other
