{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns #-}   
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Context 
    (Context(threads)
    , singleton
    , empty
    , insertVariable
    , insertBinding
    , removeVariable
    , lookupVariable
    , lookupCreator
    , lookupParticipant
    , lookupLocalTypeState
    , modifyLocalTypeStateM
    , insertParticipant
    , mapChannel
    , withChannel
    , readChannel
    , writeChannel
    , insertChannel
    , removeChannel
    , insertThread
    , removeThread
    ) where 

import Queue (Queue)
import qualified Queue
import qualified SessionType
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

import GHC.Generics
import Elm

data Context value = 
    Context
    { bindings :: Map Identifier (PID, value)
    , variableCount :: Int 
    , channels :: Map Identifier (PID, Queue Identifier)
    , threads :: Map PID Int
    -- , localTypes :: Map Identifier (SessionType.LocalType String)
    , participantMap :: Map PID Identifier
    , localTypeStates :: Map Identifier (SessionType.LocalTypeState String)
    } deriving (Generic, ElmType)

instance Show value => Show (Context value) where
    show Context { bindings, variableCount, channels, threads, participantMap, localTypeStates } = 
        "Context:"
            <> "\n"
            <> "variable count: "
            <> show variableCount
            <> "\n"
            <> Utils.showMap "bindings" bindings
            <> Utils.showMap "channels" channels
            <> Utils.showMap "threads" threads
            -- <> Utils.showMap "participants" participantMap
            <> Utils.showMap "local types" localTypeStates

singleton :: Map.Map Identifier (SessionType.LocalType String) -> Thread h a -> Context value
singleton types Thread{ pid } =
    Context 
        { bindings = Map.empty
        , variableCount = 0
        , channels = Map.empty
        , threads = Map.singleton pid 0  
        , localTypeStates = Map.mapWithKey SessionType.fromLocalType types
        , participantMap = Map.empty
        } 

empty :: Context value 
empty = 
    Context 
        { bindings = Map.empty
        , variableCount = 0
        , channels = Map.empty
        , threads = Map.empty 
        , localTypeStates = Map.empty
        , participantMap = Map.empty
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

lookupper :: (MonadState (Context value) m, MonadError Error m) => (Context value -> Map Identifier v) -> Identifier -> m v
lookupper getter key = do
    context <- State.get
    case Map.lookup key (getter context) of
        Nothing ->
            Except.throwError (UndefinedVariable key)

        Just v -> 
            return v 

{-| Get the value for an identifier from the global scope

throws an Error when the identifier is not defined
-} 
lookupVariable :: (MonadState (Context value) m, MonadError Error m) => Identifier -> m value
lookupVariable identifier = 
    snd <$> lookupper bindings identifier


lookupLocalTypeState :: (MonadState (Context value) m, MonadError Error m) => Identifier -> m (SessionType.LocalTypeState String)
lookupLocalTypeState = 
    lookupper localTypeStates 


lookupParticipant :: (MonadState (Context value) m, MonadError Error m) => PID -> m Identifier 
lookupParticipant pid = do
    context <- State.get
    case Map.lookup pid (participantMap context) of
        Nothing ->
            Except.throwError undefined 

        Just v -> 
            return v 


-- forwardWithReceive :: LocalTypeState t -> Either (SessionError (LocalAtom t)) (Identifier, t, LocalTypeState t)

putLocalTypeState :: (MonadState (Context value) m, MonadError Error m) => Identifier -> SessionType.LocalTypeState String ->  m ()
putLocalTypeState identifier localTypeState = 
    State.modify $ \context ->
        let newBindings = Map.insert identifier localTypeState (localTypeStates context)
        in
            context { localTypeStates = newBindings } 

modifyLocalTypeStateM :: (MonadState (Context value) m, MonadError Error m) 
                      => PID 
                      -> (SessionType.LocalTypeState String -> m (SessionType.LocalTypeState String, a)) 
                      ->  m a
modifyLocalTypeStateM pid tagger = do
    identifier <- lookupParticipant pid
    typeState <- lookupLocalTypeState identifier
    (newTypeState, value) <- tagger typeState
    putLocalTypeState identifier newTypeState
    return value

lookupCreator :: (MonadState (Context value) m, MonadError Error m) => Identifier -> m PID
lookupCreator identifier = do
    context <- State.get
    case Map.lookup identifier (bindings context) of
        Nothing ->
            case Map.lookup identifier (channels context) of
                Nothing ->
                    Except.throwError (UndefinedVariable identifier)

                Just (pid, _) ->
                    return pid 

        Just (pid, _) ->
            return pid 


insertVariable :: MonadState (Context value) m => PID -> value -> m Identifier 
insertVariable pid value = 
    insertBinding pid (const value)

insertBinding :: MonadState (Context value) m => PID -> (Identifier -> value) -> m Identifier
insertBinding pid tagger = do
    identifier <- freshIdentifier

    let value = tagger identifier

    State.modify $ \context ->
        let newBindings = Map.insert identifier (pid, value) (bindings context)
        in
            context { bindings = newBindings } 

    return identifier

insertParticipant :: MonadState (Context value) m => PID -> Identifier -> m ()
insertParticipant pid participant = 
    State.modify $ \context ->
        let newBindings = Map.insert pid participant (participantMap context)
        in
            context { participantMap = newBindings } 
    
    

{-| Generate a guaranteed unused (fresh) new identifier -}
freshIdentifier :: MonadState (Context value) m => m Identifier 
freshIdentifier = do
    context <- State.get
    let new = 1 + variableCount context
    put (context { variableCount = new } )
    return $ Identifier $ "var" ++ show new


insertChannel :: MonadState (Context value) m => PID -> Queue Identifier -> m Identifier 
insertChannel pid value = do
    identifier <- freshIdentifier  

    State.modify $ \context ->
        let newBindings = Map.insert identifier (pid, value) (channels context)
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
        Just (pid, queue) -> 
            tagger queue

        Nothing ->
            Except.throwError $ UndefinedChannel identifier

mapChannel :: (MonadState (Context value) m) => Identifier -> (Queue.Queue Identifier -> Queue.Queue Identifier) -> m ()
mapChannel identifier tagger = 
    State.modify $ \context ->
        context { channels = Map.adjust (\(pid, v) -> (pid, tagger v)) identifier (channels context) } 


readChannel :: (MonadState (Context value) m, MonadError Error m) => PID -> Identifier -> Identifier -> String -> ChannelName -> m (Maybe Identifier)
readChannel threadName sender receiver valueType identifier = 
    let fetch = 
            withChannel identifier $ \queue ->
                case Queue.pop threadName sender receiver valueType queue of
                    Right ( first, rest ) -> do 
                        -- put the rest of the queue back into the context
                        mapChannel identifier (const rest)
                        return first

                    Left _ ->
                        Except.throwError $ BlockedOnReceive threadName  
        handler error = 
            case error of
                BlockedOnReceive _ -> return Nothing
                _ -> Except.throwError error
    in
        fmap Just fetch `Except.catchError` handler 


writeChannel :: MonadState (Context value) m => PID -> Identifier -> Identifier -> String -> ChannelName -> Identifier -> m ()  
writeChannel threadName sender receiver valueType identifier payload = 
    mapChannel identifier (Queue.push threadName sender receiver valueType payload)

