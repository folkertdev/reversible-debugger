{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns #-}   
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Context 
    (Context(threads, localTypeStates)
    , singleton
    , empty
    , insertVariable
    , insertBinding
    , insertChannel
    , removeChannel
    , removeVariable
    , lookupVariable
    , lookupCreator
    , lookupLocalTypeState
    , modifyLocalTypeStateM
    , readChannel
    , writeChannel
    , undoReadChannel
    , undoWriteChannel
    , insertThread
    , removeThread
    , currentParticipant
    , QueueError(..)
    , getChannel
    ) where 

import Queue (Queue, Transaction(..), Item(Item, payload))
import qualified Queue
import qualified Utils.Result as Result
import qualified SessionType
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Data.Thread as Thread (Thread(..))
import Data.PID as PID (PID, create, parent, child)
import Data.Actor as Actor (Participant, Actor, toList, named, push)
import Data.Identifier as Identifier (Identifier, ChannelName, create)
import Data.List as List
import Data.Monoid ((<>))

import Control.Monad.Except as Except
import Control.Monad.State as State
import qualified Utils

import GHC.Generics
import Elm
import Data.Aeson

data Context value = 
    Context
    { bindings :: Map Identifier (Participant, value)
    , variableCount :: Int 
    , channels :: Map Identifier (Participant, Queue String value)
    , threads :: Map PID Int
    , localTypeStates :: Map Participant (SessionType.LocalTypeState String)
    , globalType :: SessionType.GlobalType
    } deriving (Generic, ElmType, ToJSON, FromJSON)


instance Show value => Show (Context value) where
    show Context { bindings, variableCount, channels, threads, localTypeStates } = 
        "Context:"
            <> "\n"
            <> "variable count: "
            <> show variableCount
            <> "\n"
            <> Utils.showMap "bindings" bindings
            <> Utils.showMap "channels" channels
            <> Utils.showMap "threads" threads
            <> Utils.showMap "local types" localTypeStates


singleton :: SessionType.GlobalType -> Map.Map Participant (SessionType.LocalType String) -> Thread h a -> Context value
singleton globalType types Thread{ pid } =
    Context 
        { bindings = Map.empty
        , variableCount = 0
        , channels = Map.empty
        , threads = Map.singleton pid 0  
        , localTypeStates = Map.mapWithKey SessionType.fromLocalType types
        , globalType = globalType
        } 

empty :: Context value 
empty = 
    Context 
        { bindings = Map.empty
        , variableCount = 0
        , channels = Map.empty
        , threads = Map.empty 
        , localTypeStates = Map.empty
        , globalType = SessionType.GlobalType [] [] 
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


lookupLocalTypeState :: (MonadState (Context value) m, MonadError Error m) => Participant -> m (SessionType.LocalTypeState String)
lookupLocalTypeState key = do
    context <- State.get
    case Map.lookup key (localTypeStates context) of
        Nothing ->
            Except.throwError (UndefinedParticipant key)

        Just v -> 
            return v 

{-
lookupParticipant :: (MonadState (Context value) m, MonadError Error m) => PID -> m Identifier 
lookupParticipant pid = do
    context <- State.get
    case concatMap Actor.toList $ Map.lookup pid (participantMap context) of
        [] ->
            Except.throwError $ UndefinedParticipant pid 
 
        (v:_) -> 
            return v 
-}

-- forwardWithReceive :: LocalTypeState t -> Either (SessionError (LocalAtom t)) (Identifier, t, LocalTypeState t)

putLocalTypeState :: (MonadState (Context value) m, MonadError Error m) => Participant -> SessionType.LocalTypeState String ->  m ()
putLocalTypeState participant localTypeState = 
    State.modify $ \context ->
        let newBindings = Map.insert participant localTypeState (localTypeStates context)
        in
            context { localTypeStates = newBindings } 

modifyLocalTypeStateM :: (MonadState (Context value) m, MonadError Error m) 
                      => Actor 
                      -> (SessionType.LocalTypeState String -> m (SessionType.LocalTypeState String, a)) 
                      ->  m a
modifyLocalTypeStateM actor tagger = do 
    identifier <- currentParticipant actor 
    typeState <- lookupLocalTypeState identifier
    (newTypeState, value) <- tagger typeState
    putLocalTypeState identifier newTypeState
    return value

lookupCreator :: (MonadState (Context value) m, MonadError Error m) => Identifier -> m Participant
lookupCreator identifier = do
    context <- State.get
    case Map.lookup identifier (bindings context) of
        Nothing ->
            case Map.lookup identifier (channels context) of
                Nothing ->
                    Except.throwError (UndefinedVariable identifier)

                Just (participant, _) ->
                    return participant 

        Just (participant, _) ->
            return participant 


insertVariable :: MonadState (Context value) m => Participant -> value -> m Identifier 
insertVariable participant value = 
    insertBinding participant (const value)

insertBinding :: MonadState (Context value) m => Participant -> (Identifier -> value) -> m Identifier
insertBinding participant tagger = do
    identifier <- freshIdentifier

    let value = tagger identifier

    State.modify $ \context ->
        let newBindings = Map.insert identifier (participant, value) (bindings context)
        in
            context { bindings = newBindings } 

    return identifier

insertChannel :: MonadState (Context value) m => Participant -> Queue String value -> m Identifier
insertChannel participant queue = do
    identifier <- freshIdentifier

    State.modify $ \context ->
        let newBindings = Map.insert identifier (participant, queue) (channels context)
        in
            context { channels = newBindings } 

    return identifier

removeChannel :: MonadState (Context value) m => ChannelName -> m ()
removeChannel channelName =
    State.modify $ \context ->
        let newBindings = Map.delete channelName (channels context)
        in
            context { channels = newBindings } 




{-| Generate a guaranteed unused (fresh) new identifier -}
freshIdentifier :: MonadState (Context value) m => m Identifier 
freshIdentifier = do
    context <- State.get
    let new = 1 + variableCount context
    put (context { variableCount = new } )
    return $ Identifier.create $ "var" ++ show new


insertThread :: (MonadState (Context value) m, MonadError Error m) => PID -> Actor -> List h -> List a -> m (Thread h a)
insertThread parent actor history value = do
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
            return $ Thread child actor history value


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
        

getChannel :: (MonadState (Context value) m, MonadError Error m) => ChannelName -> m (Queue String value)
getChannel identifier = do
    channel <- Map.lookup identifier . channels <$> State.get 
    case channel of 
        Just (participant, queue) -> 
            return queue

        Nothing ->
            Except.throwError $ UndefinedChannel identifier

putChannel :: (MonadState (Context value) m, MonadError Error m) => Participant -> ChannelName -> Queue String value -> m ()
putChannel participant identifier newQueue = 
    State.modify $ \context -> 
        context { channels = Map.insert identifier (participant, newQueue) (channels context) } 


readChannel :: (Show value, MonadState (Context value) m, MonadError Error m) => Participant -> ChannelName -> m value 
readChannel receiver channelName = do
    typeState <- lookupLocalTypeState receiver
    queue <- getChannel channelName
    case performReceive receiver typeState queue of 
        Right ( value, newTypeState, newQueue ) -> do
            putChannel receiver channelName newQueue
            putLocalTypeState receiver newTypeState
            return value

        Left (QueueError (Queue.ItemMismatch {})) -> 
            Except.throwError (BlockedOnReceive receiver)

        Left e -> 
            error (show e)



undoWriteChannel :: (Show value, MonadState (Context value) m, MonadError Error m) => Participant -> ChannelName -> m (Either QueueError value)
undoWriteChannel sender channelName = do
    typeState <- lookupLocalTypeState sender
    queue <- getChannel channelName
    case performUndoSend sender typeState queue of 
        Right ( value, newTypeState, newQueue ) -> do
            putChannel sender channelName newQueue
            putLocalTypeState sender newTypeState
            return $ Right value

        Left e -> 
            return $ Left e

undoReadChannel :: ( Show value, MonadState (Context value) m, MonadError Error m  ) => Participant -> ChannelName -> value -> m (Either QueueError ())
undoReadChannel receiver channelName payload = do
    typeState <- lookupLocalTypeState receiver
    queue <- getChannel channelName

    case performUndoReceive receiver typeState payload queue of 
        Right ( newTypeState, newQueue ) -> do
            putChannel receiver channelName newQueue
            putLocalTypeState receiver newTypeState
            return $ Right ()

        Left e -> 
            return $ Left e


writeChannel :: ( MonadState (Context value) m, MonadError Error m  ) => Participant -> ChannelName -> value -> m ()  
writeChannel sender channelName payload = do
    typeState <- lookupLocalTypeState sender
    queue <- getChannel channelName
    case performSend sender payload typeState queue of 
        Right ( newTypeState, newQueue ) -> do
            putChannel sender channelName newQueue
            putLocalTypeState sender newTypeState

        Left e -> 
            error (show e)

    return ()


data QueueError 
    = ReachedEnd SessionType.ReachedEnd 
    | ReachedBegin SessionType.ReachedBegin 
    | QueueError Queue.QueueError
    | QueueRollError Queue.QueueRollError

deriving instance Show QueueError
    

performUndoSend :: (Show t, Eq t, Show a)
            => Participant 
            ->  SessionType.LocalTypeState t 
            ->  Queue t a 
            ->  Either QueueError (a, SessionType.LocalTypeState t, Queue t a)
performUndoSend sender typeState queue = do 
    ( localAtom, newLocalTypeState ) <- Result.mapError ReachedBegin $ SessionType.backward typeState
    case localAtom of 
        SessionType.Send receiver valueType -> do
            let transaction = Transaction { sender = sender, receiver = receiver, valueType = valueType } 

            (newQueue, Queue.Item{payload}) <- Result.mapError QueueRollError $ Queue.rollPush transaction queue
            pure ( payload, newLocalTypeState, newQueue )

        SessionType.Receive {} ->
            Except.throwError undefined


performUndoReceive :: (Eq t, Show t, Show a)
               => Participant 
               ->  SessionType.LocalTypeState t 
               -> a
               ->  Queue t a 
               ->  Either QueueError (SessionType.LocalTypeState t, Queue t a)
performUndoReceive receiver typeState payload queue = do 
    ( localAtom, newLocalTypeState ) <- Result.mapError ReachedBegin $ SessionType.backward typeState
    case localAtom of 
        SessionType.Receive sender valueType -> do
            let transaction = Transaction { sender = sender, receiver = receiver, valueType = valueType } 

            newQueue <- Result.mapError QueueRollError $ Queue.rollPop transaction payload queue
            pure ( newLocalTypeState, newQueue )

        SessionType.Send {} ->
            Except.throwError undefined

performReceive :: (Eq t, Show t, Show a)
               =>  Participant
               ->  SessionType.LocalTypeState t 
               ->  Queue t a 
               ->  Either QueueError (a, SessionType.LocalTypeState t, Queue t a)
performReceive receiver typeState queue = do 
    ( localAtom, newLocalTypeState ) <- Result.mapError ReachedEnd $ SessionType.forward typeState
    case localAtom of 
        SessionType.Receive sender valueType -> do
            let transaction = Transaction { sender = sender, receiver = receiver, valueType = valueType } 

            (value, newQueue) <- Result.mapError QueueError $ Queue.pop transaction queue
            pure ( value, newLocalTypeState, newQueue )

        SessionType.Send {} ->
            Except.throwError undefined

performSend :: (Show t, Eq t)
            =>  Participant
            ->  a 
            ->  SessionType.LocalTypeState t 
            ->  Queue t a 
            ->  Either QueueError (SessionType.LocalTypeState t, Queue t a)
performSend sender payload typeState queue = do 
    ( localAtom, newLocalTypeState ) <- Result.mapError ReachedEnd $ SessionType.forward typeState
    case localAtom of 
        SessionType.Send receiver valueType -> do
            let transaction = Transaction { sender = sender, receiver = receiver, valueType = valueType } 

            let newQueue = Queue.push transaction payload queue
            pure ( newLocalTypeState, newQueue )

        SessionType.Receive {} ->
            Except.throwError undefined


currentParticipant :: (MonadState (Context value) m, MonadError Error m) => Actor -> m Participant
currentParticipant actor = 
    case Actor.toList actor of
        [] -> 
            Except.throwError $ RuntimeException "actor has no participant"

        (x:_) -> 
            return x

