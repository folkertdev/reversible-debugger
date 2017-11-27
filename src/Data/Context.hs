{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}   

module Data.Context (Context(threads), singleton, empty, insertVariable, insertBinding, removeVariable, lookupVariable, mapChannel, withChannel, readChannel, writeChannel, insertChannel, removeChannel, insertThread, removeThread) where 

import Queue (Queue)
import qualified Queue
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Data.Thread as Thread (Thread(..))
import Data.PID as PID (PID, create, parent, child)
import Data.List as List
import Data.Monoid ((<>))

import Control.Monad.Except as Except
import Control.Monad.State as State
import qualified Utils



data Context value = 
    Context
    { bindings :: Map Identifier value
    , variableCount :: Int 
    , channels :: Map Identifier (Queue Identifier)
    , threads :: Map PID Int
    } 

instance Show value => Show (Context value) where
    show (Context bindings variableCount channels threads) = 
        "Context:"
            <> "\n"
            <> "variable count: "
            <> show variableCount
            <> "\n"
            <> Utils.showMap "bindings" bindings
            <> Utils.showMap "channels" channels
            <> Utils.showMap "threads" threads

singleton :: Thread h a -> Context value
singleton (Thread pid _ _) = 
    Context 
        { bindings = Map.empty
        , variableCount = 0
        , channels = Map.empty
        , threads = Map.singleton pid 0  
        } 

empty :: Context value 
empty = 
    Context 
        { bindings = Map.empty
        , variableCount = 0
        , channels = Map.empty
        , threads = Map.empty 
        } 




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


insertVariable :: MonadState (Context value) m => value -> m Identifier 
insertVariable value = 
    insertBinding (const value)

insertBinding :: MonadState (Context value) m => (Identifier -> value) -> m Identifier
insertBinding tagger = do
    identifier <- freshIdentifier

    let value = tagger identifier

    State.modify $ \context ->
        let newBindings = Map.insert identifier value (bindings context)
        in
            context { bindings = newBindings } 

    return identifier
    
    

{-| Generate a guaranteed unused (fresh) new identifier -}
freshIdentifier :: MonadState (Context value) m => m Identifier 
freshIdentifier = do
    context <- State.get
    let new = 1 + variableCount context
    put (context { variableCount = new } )
    return $ Identifier $ "var" ++ show new


insertChannel :: MonadState (Context value) m => Queue Identifier -> m Identifier 
insertChannel value = do
    identifier <- freshIdentifier  

    State.modify $ \context ->
        let newBindings = Map.insert identifier value (channels context)
        in
            context { channels = newBindings } 

    return identifier

removeChannel :: MonadState (Context value) m => Identifier -> m () 
removeChannel identifier = 
    State.modify $ \context -> 
        case Map.lookup identifier (channels context) of
            Nothing -> 
                -- variable not defined, do nothing
                context 

            Just _ -> 
                context 
                    { channels = Map.delete identifier (channels context)
                    , variableCount = variableCount context - 1 
                    } 


insertThread :: (MonadState (Context value) m, MonadError Error m) => PID -> List h -> List a -> m (Thread h a)
insertThread parent history value = do
    context <- State.get
    let usedThreadNames = threads context  
    case Map.lookup parent usedThreadNames of
        Nothing -> 
            Except.throwError $ RuntimeException "thread name without parent"

        Just childCount -> do
            let 
                child = 
                    PID.child childCount parent

                updater = 
                    Map.adjust (+ 1) parent . Map.insert child 0 

            State.put (context { threads =  updater usedThreadNames }) 
            return $ Thread child history value


removeThread :: (MonadState (Context value) m, MonadError Error m) => PID -> m () 
removeThread pid = 
    let removeT threads =
            case Map.lookup pid threads of
                Nothing -> 
                    Except.throwError $ RuntimeException $ "removing non-existent thread: "  ++ show pid 

                Just 0 -> 
                    return $ Map.delete pid threads

                Just n -> 
                    Except.throwError $ RuntimeException $ "removing non-empty thread, it has # of remaining children: "  ++ show n

        decrementParent threads = 
            let parent = 
                    PID.parent pid
            in
                Map.adjust (\v -> v - 1) parent threads
    in do
        context <- State.get 
        let ts = threads context
        newTs <- decrementParent <$> removeT ts 
        State.put context{ threads = newTs }
        

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

