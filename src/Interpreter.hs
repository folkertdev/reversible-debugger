{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (forward, backward, rollVariable, rollThread, Interpreter.init, activeInactiveThreads, Task) where

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
import Control.Monad.Free
import Control.Comonad.Cofree
import qualified Control.Exception as Exception (catch, BlockedIndefinitelyOnMVar)
import qualified System.Timeout
import Text.ParserCombinators.Parsec as Parsec
import Data.Foldable (Foldable)



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

data Task a 
    = Singleton (Thread a)
    | Parallel (Task a) (Task a)
    deriving (Show)


data Thread a = Thread ThreadName (List History) (List a) deriving (Show)

listThreads :: Task a -> List (Thread a)
listThreads task = 
    case task of
        Parallel a b -> 
            listThreads a ++ listThreads b


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
insertVariable identifier value ref = liftIO $ do
    map <- takeMVar ref
    putMVar ref (Map.insert identifier value map)


removeVariable :: Identifier -> MVar (Map.Map Identifier Value) -> IOThrowsError ()
removeVariable identifier ref = liftIO $ do
    map <- takeMVar ref
    putMVar ref (Map.delete identifier map)


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


embedEither :: Monad m => Either e a  -> ExceptT e m a
embedEither v = ExceptT (return v)


{-| Run two tasks in parallel -} 
parallel :: (Task a -> IOThrowsError (Task a)) -> Task a -> Task a -> IOThrowsError (Task a)
parallel tagger left right = do
    leftContainer <- liftIO newEmptyMVar 

    liftIO $ forkIO $ do
        result <- runExceptT $ tagger left
        putMVar leftContainer result

    result2 <- liftIO $ runExceptT $ tagger right
    result1 <- liftIO $ readMVar leftContainer

    embedEither $ liftA2 Parallel result1 result2

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

{-| Evaluate a program one step forward -} 
forward :: MVar Int -> MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Task Program -> IOThrowsError (Task Program)
forward identifierRef bindings threadNames task = 
    case task of 
        Parallel task1 task2 -> 
            case (task1, task2) of 
                (Singleton (Thread _ _ []), _) ->
                    return task2 

                (_, Singleton (Thread _ [] _)) ->
                    return task1

                _ -> 
                    parallel (forward identifierRef bindings threadNames) task1 task2 
            


        Singleton (Thread name history []) -> 
            return task 

        Singleton (Thread name history (program:rest)) ->
            let 
                continue :: History -> List Program -> IOThrowsError (Task Program)
                continue historyInstruction instructions = 
                    return $ Singleton (Thread name (historyInstruction : history) instructions)
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
                    return $ Parallel
                        (Singleton $ Thread name (SpawnedThread threadName : history) rest)
                        (Singleton $ Thread threadName [] [ work ])

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


 
rollThread :: ThreadName -> MVar (Map Identifier Value) -> MVar (Map ThreadName Int) -> Task Program -> IOThrowsError (Task Program)
rollThread threadName bindings threadNames task = 
    let 
        recurse task = 
            rollThread threadName bindings threadNames =<< backward bindings threadNames task
    in
    case task of
        Parallel parent child -> do
            -- roll the child first
            newChild <- recurse child
            newParent <- recurse parent

            return $ Parallel newParent newChild 

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
        Parallel task1 task2 ->
            case (task1, task2) of
                ( Singleton (Thread name ( SpawnedThread threadName : restOfHistory) restOfProgram), Singleton (Thread spawnedName [] [ spawnedProgram ])) ->
                    return $ Singleton $ Thread name restOfHistory $ SpawnThread spawnedProgram : restOfProgram

                _ ->
                    parallel (backward bindings threadNames) task1 task2 

        Singleton (Thread name [] program) ->
            return task

        Singleton (Thread name (mostRecent:restOfHistory) program) ->
            let 
                continue :: List Program -> IOThrowsError (Task Program)
                continue = return . Singleton . Thread name restOfHistory
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

                -- undoing the send of a value requires to undo the receive of the same value, if performed and not yet undone
                ( Sent channelName, restOfProgram ) -> do
                    -- reverse of send is receive
                    channel <- lookupChannel channelName bindings
                    -- message <- liftIO $ readChan channel 
                    message <- readChannelWithTimeout name channel 200 -- liftIO $ readChan channel

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
