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

withDefault :: a -> Maybe a -> a
withDefault = 
    fromMaybe 



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
freshIdentifier :: Interpreter value Identifier
freshIdentifier = do
    context <- State.get
    let new = 1 + variableCount context
    put (context { variableCount = new } )
    return $ Identifier $ "var" ++ show new


freshThreadName :: ThreadName -> Interpreter value ThreadName
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
            context { _bindings = newBindings, variableCount = variableCount context - 1 } 


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



readChannel :: ThreadName -> Identifier -> Interpreter value Identifier
readChannel threadName identifier = 
    withChannel identifier $ \queue ->
        case Queue.pop queue of
            Just ( first, rest ) -> do 
                -- put the rest of the queue back into the context
                mapChannel identifier (\_ -> rest)
                return first

            Nothing ->
                throw $ BlockedOnReceive threadName  


writeChannel :: ThreadName -> Identifier -> Identifier -> Interpreter value ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push payload)



{-| Puts an either into an ExceptT context: Left throws an error, Right
continues the program
-} 
embedEither :: Monad m => Either e a  -> ExceptT e m a
embedEither v = ExceptT (return v)

{-| Revert the program state before the creation of the given variable -}
rollVariable :: ReversibleLanguage program => Identifier -> Task program -> Interpreter (Value program) (Task program)
rollVariable name task = do 
    lookupVariable name -- will throw if the name does not exist
    case task of
        Singleton (Thread _ (mostRecent : restOfHistory) program) ->
            case createdVariable mostRecent of
                Nothing ->
                    rollVariable name =<< backward task

                Just identifier ->
                    if name == identifier then  
                        backward task

                    else
                        rollVariable name =<< backward task

        _ ->
            rollVariable name =<< backward task


{-| Revert a whole thread -} 
rollThread :: ReversibleLanguage program => ThreadName -> Task program -> Interpreter (Value program) (Task program)
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

            Singleton (Thread parentName (mostRecent : restOfHistory) parentProgram) -> 
                case spawned mostRecent of
                    Nothing -> 
                        if parentName == threadName then
                            -- this is the thread we want to unroll
                            recurse task
                        else
                            return task

                    Just spawnedName -> 
                        if spawnedName == threadName then
                            -- undo the spawning, the rest is undone
                            backward task

                        else
                            recurse task


