{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Session where 

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Arrow (first, second)

import Data.Map as Map

import LocalType (LocalTypeState, LocalType, Participant, Location, Identifier)
import Program (Program, terminate, Value(..), IntOperator(..))
import Queue (Queue, QueueError)
import qualified Queue
import Utils (List)

type Session value a = StateT (ExecutionState value) (Except Error) a


data ExecutionState value = 
    ExecutionState 
        { variableCount :: Int
        , locationCount :: Int
        , applicationCount :: Int
        , participants :: Map Participant (Monitor value String)
        , locations :: Map Location (Participant, Program value)
        , queue :: Queue value
        , isFunction :: value -> Maybe (Identifier, Program value)
        }

instance Show v => Show (ExecutionState v) where 
    show state = 
        "locations: " ++ show (locations state) 
            ++ "\n\n\n" 
            ++ "participants: " ++ show (participants state)
            ++ "\n\n\n" 
            ++ "queue: " ++ show (queue state)

instance Eq v => Eq (ExecutionState v) where 
    a == b = locations a == locations b && participants a == participants b && queue a == queue b

data Monitor value tipe = 
    Monitor 
        { _localType :: LocalTypeState (Program value) value tipe
        , _recursiveVariableNumber :: Int
        , _recursionPoints :: List (LocalType tipe)
        , _store :: Map Identifier value 
        , _applicationHistory :: Map Identifier (Identifier, value)
        }
        deriving (Show, Eq)



data Error 
    = UndefinedParticipant Participant
    | UndefinedVariable Participant Identifier
    | SynchronizationError String
    | LabelError String
    | QueueError String Queue.QueueError
    deriving (Eq, Show)


ensure condition error = 
    if condition then 
        return () 
    else 
        Except.throwError error


{-| Evaluate a value until a base type, by dereferencing references -}
evaluateValue :: Participant -> Value -> Session Value Value 
evaluateValue participant value = 
    case value of 
        VReference identifier -> do 
            store <- _store <$> getMonitor participant
            case Map.lookup identifier store of
                Nothing -> 
                    let message = 
                            "The variable `" ++ identifier ++ "` is undefined for participant `" ++ participant ++ "`"
                    in error message 
                Just v -> 
                    evaluateValue participant v

        VIntOperator a operator b -> do
            VInt left <- evaluateValue participant a
            VInt right <- evaluateValue participant b
            case operator of 
                Add -> 
                    pure $ VInt (left + right)

        VComparison a expected b -> do
            verdict <- liftM2 compare (evaluateValue participant a) (evaluateValue participant b)
            pure $ VBool (verdict == expected)
            
            
        _ -> 
            return value



setParticipant :: Location -> Participant -> (Monitor value String, Program value) -> Session value ()
setParticipant location participant (monitor, program) = 
    State.modify $ \state@ExecutionState { participants, locations } -> 
        state 
            { participants = Map.insert participant monitor participants
            , locations = Map.insert location (participant, program) locations
            }

setMonitor :: Location -> Participant -> Monitor value String -> Session value ()
setMonitor location participant monitor = 
    State.modify $ \state@ExecutionState { participants, locations } -> 
        state 
            { participants = Map.insert participant monitor participants
            }

getParticipant :: Location -> Participant -> Session value (Monitor value String, Program value)
getParticipant location participant = do
    state <- State.get
    monitor <- getMonitor participant
    case Map.lookup location (locations state) of 
        Nothing -> 
            error "location or participant does not exist"

        Just (_, program) -> 
            return ( monitor, program )

removeLocation :: Location -> Session value () 
removeLocation location = 
    State.modify $ \state@ExecutionState { locations } -> 
        state 
            { locations = Map.delete location locations
            }



lookupVariable :: Participant -> Identifier -> Session value value
lookupVariable participant identifier = do 
    monitor <- getMonitor participant 
    case Map.lookup identifier (_store monitor) of 
        Nothing -> 
            Except.throwError $ UndefinedVariable participant identifier

        Just value -> 
            return value

uniqueVariableName :: Session value Identifier
uniqueVariableName = do
    count <- variableCount <$> State.get 

    State.modify $ \state@ExecutionState { variableCount } -> 
        state { variableCount = count + 1 } 

    return ("v" ++ show count)

uniqueApplicationName :: Session value Identifier
uniqueApplicationName = do
    count <- applicationCount <$> State.get 

    State.modify $ \state@ExecutionState { applicationCount } -> 
        state { applicationCount = count + 1 } 

    return ("k" ++ show count)

uniqueLocation :: Session value Location
uniqueLocation = do
    count <- locationCount <$> State.get 

    State.modify $ \state@ExecutionState { locationCount } -> 
        state { locationCount = count + 1 } 

    return ("l" ++ show count)


getMonitor :: Participant -> Session value (Monitor value String)
getMonitor participant = do 
    ps <- participants <$> State.get

    case Map.lookup participant ps of
        Nothing -> 
            Except.throwError $ UndefinedParticipant participant

        Just monitor -> 
            return monitor 



putMonitor :: Participant -> Monitor Value String -> Session Value ()
putMonitor participant newMonitor = 
    State.modify $ \state -> 
        state { participants = Map.insert participant newMonitor (participants state) }


-- Queue Actions 


pushToQueue :: (Participant, Participant, value) -> Session value () 
pushToQueue value = 
    liftQueue undefined $ Queue.push value


rollQueueHistory :: String -> Participant -> Participant -> Session value value
rollQueueHistory context a b = 
    liftQueue context $ Queue.popHistory a b


{-| Pops an item from the queue, and adds it to the history -}
popFromQueue :: String -> Participant -> Participant -> Session value value
popFromQueue context a b = 
    liftQueue context $ Queue.pop a b

popLabelFromQueue :: String -> Participant -> Participant -> Session Value String
popLabelFromQueue context a b = do
    result <- liftQueue context $ Queue.pop a b
    case result of 
        VLabel label -> 
            return label

        _ -> 
            liftQueue context $ Except.throwError $ Queue.InvalidQueueItem $ "Expecting a label, but got " ++ show result 


{-| Pops an item from the queue, but does _not_ add it to the history -} 
removeFromQueue :: String -> Participant -> Participant -> Session value value
removeFromQueue context a b = 
    liftQueue context $ Queue.remove a b


liftQueue :: String -> StateT (Queue value) (Except QueueError) a -> Session value a 
liftQueue context small = do
    state@ExecutionState{queue} <- State.get

    case Except.runExcept $ State.runStateT small queue of 
        Left error -> 
            Except.throwError $ QueueError context error

        Right (value, newQueue) -> do
            State.put $ state { queue = newQueue }
            return value
