{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}   

module Data.Context where 

import Queue
import Data.Map (Map)
import Data.Map as Map
import Types
import Data.Thread as Thread (Thread(..))

import Control.Monad.Except as Except
import Control.Monad.State as State


data Context value = 
    Context
    { bindings :: Map Identifier value
    , variableCount :: Int 
    , channels :: Map Identifier (Queue Identifier)
    , threads :: Map PID Int
    } 

insertVariable :: Identifier -> value -> Context value -> Context value 
insertVariable identifier value context = 
        let 
            newBindings = Map.insert identifier value (bindings context)
        in
            context { bindings = newBindings } 


removeVariable :: Identifier -> Context value -> Context value  
removeVariable identifier context = 
    let 
        variables = bindings context 
    in 
        case Map.lookup identifier variables of
            Nothing -> 
                -- variable not defined, do nothing
                context 

            Just _ -> 
                let 
                    newBindings = Map.delete identifier variables 
                in
                    context { bindings = newBindings, variableCount = variableCount context - 1 } 


{-| Get the value for an identifier from the global scope

throws an Error when the identifier is not defined
-} 
lookupVariable :: (MonadState (Context value) m, MonadError Error m) => Identifier -> m value
lookupVariable identifier = do
    context <- State.get
    case Map.lookup identifier (bindings context) of
        Nothing ->
            Except.throwError (UndefinedVariable identifier)

        Just v ->
            return v


{-| Generate a guaranteed unused (fresh) new identifier -}
freshIdentifier :: MonadState (Context value) m => m Identifier 
freshIdentifier = do
    context <- State.get
    let new = 1 + variableCount context
    put (context { variableCount = new } )
    return $ Identifier $ "var" ++ show new


freshThreadName :: (MonadState (Context value) m, MonadError Error m) => PID -> m PID 
freshThreadName parentName = do
    context <- State.get
    let usedThreadNames = threads context  
    case Map.lookup parentName usedThreadNames of
        Nothing -> 
            Except.throwError $ RuntimeException "thread name without parent"

        Just childCount -> do
            let 
                childName = 
                    parentName ++ [childCount + 1]

                updater = 
                    Map.adjust (+ 1) parentName . Map.insert childName 0 

            State.put (context { threads =  updater usedThreadNames }) 
            return childName


withChannel :: (MonadState (Context value) m, MonadError Error m) => Identifier -> (Queue.Queue Identifier -> m a) -> m a 
withChannel identifier tagger = do
    channel <- Map.lookup identifier . channels <$> State.get 
    case channel of 
        Just queue -> 
            tagger queue

        Nothing ->
            Except.throwError $ UndefinedChannel identifier

mapChannel :: (MonadState (Context value) m) => Identifier -> (Queue.Queue Identifier -> Queue.Queue Identifier) -> m ()
mapChannel identifier tagger = 
    State.modify $ \context ->
        context { channels = Map.adjust tagger identifier (channels context) } 


readChannel :: (MonadState (Context value) m, MonadError Error m) => PID -> Identifier -> m (Maybe Identifier)
readChannel threadName identifier = 
    let fetch = 
            withChannel identifier $ \queue ->
                case Queue.pop threadName queue of
                    Just ( first, rest ) -> do 
                        -- put the rest of the queue back into the context
                        mapChannel identifier (const rest)
                        return first

                    Nothing ->
                        Except.throwError $ BlockedOnReceive threadName  
        handler error = 
            case error of
                BlockedOnReceive _ -> return Nothing
                _ -> Except.throwError error
    in
        fmap Just fetch `Except.catchError` handler 


writeChannel :: MonadState (Context value) m => PID -> Identifier -> Identifier -> m ()  
writeChannel threadName identifier payload = 
    mapChannel identifier (Queue.push threadName payload)

{-
rollWriteChannel :: (MonadState (Context value) m, MonadError Error m) => PID -> Identifier -> m () 
rollWriteChannel pid channelName = 
    withChannel channelName $ \channel ->  
        case Queue.tryRevertPush pid channel of
            Right newQueue -> 
                State.modify $ \context ->
                    context { channels = Map.adjust (const newQueue) channelName (channels context) } 
                 
            Left error -> 
                Except.throwError $ RuntimeException (show error)
rollReadChannel :: (MonadState (Context value) m, MonadError Error m) => PID -> Identifier -> Identifier -> m () 
rollReadChannel pid channelName payload = 
    withChannel channelName $ \channel -> 
        case Queue.tryRevertPop pid payload channel of
            Right newQueue -> 
                State.modify $ \context ->
                    context { channels = Map.adjust (const newQueue) channelName (channels context) } 
                 
            Left error -> 
                Except.throwError $ RuntimeException (show error)

-}
