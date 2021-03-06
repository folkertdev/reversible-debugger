{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Session where 

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Arrow (first, second)

import Data.Map as Map

import LocalType (LocalType, Participant, Location, Identifier)
import TypeContext (LocalTypeState) 
import Program (Program, terminate, Value(..), IntOperator(..))
import Queue (Queue, QueueError)
import qualified Queue
import Utils (List, (|>))
import qualified Utils.Maybe as Maybe
import Zipper (Zipper)
import qualified Zipper 

type Session value a = StateT (ExecutionState value) (Except Error) a


data ExecutionState value = 
    ExecutionState 
        { variableCount :: Int
        , locationCount :: Int
        , applicationCount :: Int
        , participants :: Map Participant (Monitor value String)
        , locations :: Map Location (Participant, List OtherOptions, Program value)
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

data OtherOptions  
    = OtherSelections (Zipper (String, Value, Program Value))
    | OtherOffers (Zipper (String, Program Value))
    | OtherBranch Value Bool (Program Value)
    deriving (Show, Eq)

data Monitor value tipe = 
    Monitor 
        { _localType :: LocalTypeState tipe
        , _recursiveVariableNumber :: Int
        , _recursionPoints :: List (LocalType tipe)
        , _store :: Map Identifier value 
        , _usedVariables :: List Binding 
        , _applicationHistory :: Map Identifier (value, value)
        }
        deriving (Show, Eq)

data Binding = Binding { _visibleName :: Identifier, _internalName :: Identifier } deriving (Show, Eq) 

markVariableAsUsed :: Identifier -> Identifier -> Monitor a b -> Monitor a b
markVariableAsUsed visible internal monitor@Monitor{_usedVariables} = 
    monitor { _usedVariables = Binding visible internal : _usedVariables }

addApplication :: (Identifier, (Value, Value)) -> Monitor Value a -> Monitor Value a 
addApplication (k, v) monitor@Monitor{_applicationHistory} = 
    monitor { _applicationHistory = Map.insert k v _applicationHistory }

{-
storeSelectOtherOptions :: Zipper (String, Value, Program Value) -> Monitor Value b -> Monitor Value b
storeSelectOtherOptions var monitor@Monitor{_choiceOtherOptions} = 
    monitor { _choiceOtherOptions = Left var : _choiceOtherOptions }

storeOfferOtherOptions :: Zipper (String, Program Value) -> Monitor Value b -> Monitor Value b
storeOfferOtherOptions var monitor@Monitor{_choiceOtherOptions} = 
    monitor { _choiceOtherOptions = Right var : _choiceOtherOptions }
-}


data Error 
    = UndefinedParticipant Participant
    | UndefinedVariable Participant Identifier
    | SynchronizationError String
    | LabelError String
    | QueueError String Queue.QueueError
    | ChoiceError ChoiceError
    | Terminated
    deriving (Show, Eq)

data Choice = S | O deriving (Eq)

instance Show Choice where 
    show S = "Select"
    show O = "Offer"

data ChoiceError 
    = InvalidOwner { choice :: Choice,  got :: Participant, expected :: Participant } 
    | NotChoiceInstruction { choice :: Choice, owner :: Participant, localType :: LocalType String } 
    deriving (Eq)

instance Show ChoiceError where
    show e = 
        case e of 
            InvalidOwner { choice, got, expected } -> 
                show choice ++ "'s owners don't match: got " ++ got ++ " but the type expects " ++ expected 

            NotChoiceInstruction { choice, owner, localType } ->  
                show choice ++ " for owner " ++ owner ++ " needs a LocalType." ++ show choice ++ ", but got " ++ show localType



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


setParticipant :: Location -> Participant -> (Monitor value String, List OtherOptions, Program value) -> Session value ()
setParticipant location backupParticipant (monitor, otherOptionsStack, program) =
    State.modify $ \state@ExecutionState { participants, locations } -> 
        let defaultParticipant = 
                locations
                    |> Map.lookup location 
                    |> fmap (\(p, _,_) -> p) 
                    |> Maybe.withDefault backupParticipant
        in
        state 
            { participants = Map.insert backupParticipant monitor participants
            , locations = Map.insert location (defaultParticipant, otherOptionsStack, program) locations
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

        Just (_, _, program) -> 
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
