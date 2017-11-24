{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, Rank2Types, NamedFieldPuns #-}
module Interpreter 
    (Thread(..)
    , Context
    , Interpreter
    , forward
    , backward
    , lookupVariable
    , freshIdentifier
    , insertVariable
    , readChannel
    , writeChannel
    , freshThreadName
    , removeVariable
    , rollVariable
    , rollThread
    , advanceThread
    , rollReadChannel
    , rollWriteChannel
    , rollReceive
    , rollSend
    ) where

{-| The main body of code

The interesting stuff happens in the backward and forward functions.

-}

import Prelude hiding (head)
import Types
import Data.ReversibleLanguage as ReversibleLanguage
import qualified Queue
import Queue (QueueHistory(..))

import qualified Data.Map as Map 
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State as State 
import Control.Applicative (Applicative, liftA2, pure, (<*), (<|>))
import Control.Monad
import Data.Foldable (Foldable, foldrM)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import Debug.Trace

withDefault :: a -> Maybe a -> a
withDefault = 
    fromMaybe 


{-| Generate a guaranteed unused (fresh) new identifier -}
freshIdentifier :: Interpreter value Identifier
freshIdentifier = do
    context <- State.get
    let new = 1 + _variableCount context
    put (context { _variableCount = new } )
    return $ Identifier $ "var" ++ show new


freshThreadName :: PID -> Interpreter value PID
freshThreadName parentName = do
    context <- State.get
    let usedThreadNames = _threads context  
    case Map.lookup parentName usedThreadNames of
        Nothing -> 
            throw $ RuntimeException "thread name without parent"

        Just childCount -> do
            let 
                childName = 
                    parentName ++ [childCount + 1]

                updater = 
                    Map.adjust (+ 1) parentName . Map.insert childName 0 

            State.put (context { _threads =  updater usedThreadNames }) 
            return childName


insertVariable :: Identifier -> value -> Interpreter value () 
insertVariable identifier value = 
    State.modify $ \context -> 
        let 
            newBindings = Map.insert identifier value (_bindings context)
        in
            context { _bindings = newBindings } 


removeVariable :: Identifier -> Interpreter value ()  
removeVariable identifier = 
    State.modify $ \context -> 
        let 
            newBindings = Map.delete identifier (_bindings context)
            
        in
            context { _bindings = newBindings, _variableCount = _variableCount context - 1 } 


{-| Get the value for an identifier from the global scope

throws an Error when the identifier is not defined
-} 
lookupVariable :: Identifier -> Interpreter value value 
lookupVariable identifier = do
    map <- _bindings <$> State.get
    case Map.lookup identifier map of
        Nothing ->
            throw (UndefinedVariable identifier)

        Just v ->
            return v

withChannel :: Identifier -> (Queue.Queue Identifier -> Interpreter value a) -> Interpreter value a
withChannel identifier tagger = do
    channel <- Map.lookup identifier . _channels <$> State.get 
    case channel of 
        Just queue -> 
            tagger queue

        Nothing ->
            throw $ UndefinedChannel identifier

mapChannel :: Identifier -> (Queue.Queue Identifier -> Queue.Queue Identifier) -> Interpreter value ()
mapChannel identifier tagger = 
    State.modify $ \context ->
        context { _channels = Map.adjust tagger identifier (_channels context) } 


readChannel :: PID -> Identifier -> Interpreter value (Maybe Identifier)
readChannel threadName identifier = 
    let fetch = 
            withChannel identifier $ \queue ->
                case Queue.pop threadName queue of
                    Just ( first, rest ) -> do 
                        -- put the rest of the queue back into the context
                        mapChannel identifier (const rest)
                        return first

                    Nothing ->
                        throw $ BlockedOnReceive threadName  
        handler error = 
            case error of
                BlockedOnReceive _ -> return Nothing
                _ -> throw error
    in
        fmap Just fetch `catch` handler 

rollReadChannel :: PID -> Identifier -> Identifier -> Interpreter value () 
rollReadChannel pid channelName payload = 
    withChannel channelName $ \channel -> 
        case Queue.tryRevertPop pid payload channel of
            Right newQueue -> 
                State.modify $ \context ->
                    context { _channels = Map.adjust (const newQueue) channelName (_channels context) } 
                 
            Left error -> 
                throw $ RuntimeException (show error)


writeChannel :: PID -> Identifier -> Identifier -> Interpreter value ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push threadName payload)


rollWriteChannel :: PID -> Identifier -> Interpreter value () 
rollWriteChannel pid channelName = 
    withChannel channelName $ \channel -> 
        case Queue.tryRevertPush pid channel of
            Right newQueue -> 
                State.modify $ \context ->
                    context { _channels = Map.adjust (const newQueue) channelName (_channels context) } 
                 
            Left error -> 
                throw $ RuntimeException (show error)



{-| Puts an either into an ExceptT context: Left throws an error, Right
continues the program
-} 
embedEither :: Monad m => Either e a  -> ExceptT e m a
embedEither v = ExceptT (return v)

head list = 
    case list of
        [] -> Nothing
        (x:_) -> Just x

rollReceive :: ReversibleLanguage program => Identifier -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program) 
rollReceive channelName thread threads =
    withChannel channelName $ \channel -> 
        case Queue.mostRecentAction channel of
            Nothing -> 
                return $ Right ( thread, threads ) 

            Just (Added pid)  -> do
                -- first roll a send, then try to roll receive
                rolledSend <- rollSend channelName thread threads
                case rolledSend of 
                    Left done -> return $ Left done 
                    Right (t, ts) -> 
                        rollReceive channelName t ts 
                


            Just (Removed pid) ->
                -- unroll until the receive is reverted
                case scheduleThread pid thread threads of 
                    Left error -> 
                        throw $ ThreadScheduleError pid error

                    Right (current, state) ->
                        go (predicate pid channelName) current state

go predicate thread threads = 
    if predicate thread then
        return $ Right (thread, threads) 
    else do
        oneStepBack <- backward thread threads
        case oneStepBack of
            Left done -> return $ Left done
            Right (t, ts) -> 
                go predicate t ts 


predicate onThread onChannel thread@(Thread pid history _) = 
    let receivedOnCorrectChannel =
            case received =<< head history of
                Nothing -> False 
                Just receivedOn -> 
                    receivedOn == onChannel
    in 
        onThread == pid && receivedOnCorrectChannel 

                
rollSend :: ReversibleLanguage program => Identifier -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program) 
rollSend channelName thread threads =
    withChannel channelName $ \channel -> 
        case Queue.mostRecentAction channel of
            Nothing -> 
                return $ Right ( thread, threads ) 

            Just (Added pid)  -> 
                -- unroll until the send is reverted
                case scheduleThread pid thread threads of 
                    Left error -> 
                        throw $ ThreadScheduleError pid error

                    Right (current, state) ->
                        go (predicate pid channelName) current state

            Just (Removed pid) -> do
                -- first roll a receive, then try to roll receive
                rolledReceive <- rollReceive channelName thread threads
                case rolledReceive of 
                    Left done -> return $ Left done 
                    Right (t, ts) -> 
                        rollSend channelName t ts 
    


{-| Revert the program state before the creation of the given variable -}
rollVariable :: ReversibleLanguage program => Identifier -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program) 
rollVariable name thread@(Thread pid history program) state@ThreadState{active, inactive, blocked, filtered} = do 
    let 
        recurse = do
            oneStepBack <- backward thread state
            case oneStepBack of
                Left done -> return $ Left done
                Right (newThread, newState) -> 
                    rollVariable name newThread newState
    lookupVariable name -- will throw if the name does not exist
    case history of
        [] -> 
            -- nothing to revert 
            return $ Left (state { inactive = Map.insert pid thread inactive }) 

        (mostRecent : restOfHistory) -> 
            case createdVariable mostRecent of
                Nothing -> 
                    recurse

                Just identifier ->
                    if name == identifier then  
                         -- roll the assignment
                         backward thread state
                    else
                        recurse


parent :: ReversibleLanguage program => Thread program -> ThreadState program -> Interpreter (Value program) (Maybe (Thread program, ThreadState program))
parent child@(Thread pid _ _) state@ThreadState{active, inactive, blocked, filtered} = 
    let 
        -- we only actually return the parent when it is valid
        validThreads = mconcat [ active, inactive, blocked, blocked ]
        parentID = Prelude.init pid
    in
        return $ 
            case Map.lookup parentID validThreads of
                Nothing -> 
                    Nothing
                Just parent -> 
                    Just 
                        ( parent
                        , ThreadState 
                              (Map.insert pid child (Map.delete parentID active))
                              (Map.delete parentID inactive) 
                              (Map.delete parentID blocked)
                              (Map.delete parentID filtered)
                        )
 


{-| Revert a whole thread -} 
rollThread  :: ReversibleLanguage program => PID -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program) 
rollThread pid thread@(Thread parentId history program) state@ThreadState{ active, inactive, blocked, filtered } = do
    let recurse task = do 
            oneStepBack <- backward thread state
            case oneStepBack of
                Left done -> return $ Left done
                Right (newThread, newState) -> 
                    rollThread pid (traceShowId newThread) newState

    exists <- Map.member pid . _threads <$> State.get
    if traceShowId $ not exists then
        throw $ UndefinedThread pid
    else if not (parentId `isPrefixOf` pid) then do
        -- rolling thread is not an ancestor of the thread we want to roll
        -- so try to schedule the parent
        result <- parent thread state 
        case result of
            Nothing -> 
                error "cannot find the parent thread, so cannot roll"
            Just ( parentThread, newState ) -> 
                rollThread  pid parentThread newState 
    else
        case history of
            [] -> 
                -- nothing to revert 
                return $ Left (state { inactive = Map.insert pid thread inactive }) 

            (mostRecent : restOfHistory) ->
                case spawned mostRecent of
                    Nothing -> 
                        recurse thread 

                    Just spawnedName -> 
                        if spawnedName == pid then
                            -- undo the spawning, the rest is undone
                            backward thread state

                        else
                            recurse thread

advanceThread :: ReversibleLanguage program => PID -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program)
advanceThread pid current state =  
    case scheduleThread pid current state of 
        Left error -> 
            throw $ ThreadScheduleError pid error

        Right (current, state) -> 
            forward current state



        
                
        
    

