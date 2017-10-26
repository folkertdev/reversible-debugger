{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module Interpreter (Thread(..), Task(..), Context, Interpreter, forward, backward, activeInactiveThreads, lookupVariable, freshIdentifier, insertVariable, readChannel, writeChannel, freshThreadName, removeVariable, rollVariable, rollThread) where

{-| The main body of code

The interesting stuff happens in the backward and forward functions.

-}

import Types
import ReversibleLanguage 
import qualified Queue

import qualified Data.Map as Map 
import Data.Map (Map)
import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans.State as State 
import Control.Applicative (Applicative, liftA2, pure, (<*))
import Control.Monad
import Data.Foldable (Foldable, foldrM)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

withDefault :: a -> Maybe a -> a
withDefault = 
    fromMaybe 



listThreads :: Task (Thread a) -> List (Thread a)
listThreads task = 
    case task of
        Parallel a b -> 
            a : concatMap listThreads b


activeInactiveThreads :: Task (Thread a) -> ( List PID, List PID ) 
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
                case Queue.pop queue of
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


writeChannel :: PID -> Identifier -> Identifier -> Interpreter value ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push payload)



{-| Puts an either into an ExceptT context: Left throws an error, Right
continues the program
-} 
embedEither :: Monad m => Either e a  -> ExceptT e m a
embedEither v = ExceptT (return v)

{-| Revert the program state before the creation of the given variable -}
rollVariable :: ReversibleLanguage program => Identifier -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program) 
rollVariable name thread@(Thread pid history program) state@(active, inactive, blocked, filtered) = do 
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
            return $ Left (active, Map.insert pid thread inactive, blocked, filtered)

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


{-| Revert a whole thread -} 
rollThread  :: ReversibleLanguage program => PID -> Thread program -> ThreadState program -> Interpreter (Value program) (ExecutionState program) 
rollThread pid thread@(Thread parentId history program) state@(active, inactive, blocked, filtered) = do
    let recurse task = do 
            oneStepBack <- backward thread state
            case oneStepBack of
                Left done -> return $ Left done
                Right (newThread, newState) -> 
                    rollThread pid newThread newState

    exists <- Map.member pid . _threads <$> State.get
    if not exists then
        throw $ UndefinedThread pid
    else if not (parentId `isPrefixOf` pid) then
        -- rolling thread is not an ancestor of the thread we want to roll
        error "thread to unroll is no child of the given thread"
    else
        case history of
            [] -> 
                -- nothing to revert 
                return $ Left (active, Map.insert pid thread inactive, blocked, filtered)

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


