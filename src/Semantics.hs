{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, DuplicateRecordFields, NamedFieldPuns, DeriveAnyClass, StandaloneDeriving, UndecidableInstances  #-}
module Semantics where 

import Control.Monad.State as State
import Control.Monad.Except as Except

import LocalType (LocalType, LocalTypeState, Location, Participant, Identifier)
import qualified LocalType

import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix

import Types ((|>))

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

type List = []

type Session value a = StateT (ExecutionState value) (Either Error) a

data ExecutionState value = 
    ExecutionState 
        { variableCount :: Int
        , locationCount :: Int
        , applicationCount :: Int
        , participants :: Map Participant (Monitor value String)
        , locations :: Map Location (Map Participant (Program value))
        , queue :: Queue value
        , isFunction :: value -> Maybe (Identifier, Program value)
        }

lookupParticipant :: Participant -> Session value (Monitor value String)
lookupParticipant participant = do 
    ps <- participants <$> State.get

    case Map.lookup participant ps of
        Nothing -> 
            Except.throwError $ UndefinedParticipant participant

        Just monitor -> 
            return monitor 


setParticipant :: Location -> Participant -> (Monitor value String, Program value) -> Session value ()
setParticipant location participant (monitor, program) = 
    State.modify $ \state@ExecutionState { participants, locations } -> 
        state 
            { participants = Map.insert participant monitor participants
            , locations = Map.update (Just . Map.insert participant program) location locations
            }

getParticipant :: Location -> Participant -> Session value (Monitor value String, Program value)
getParticipant location participant = do
    state <- State.get
    monitor <- lookupParticipant participant
    case Map.lookup location (locations state) >>= Map.lookup participant of 
        Nothing -> 
            error "location or participant does not exist"

        Just program -> 
            return ( monitor, program )

removeParticipant :: Location -> Participant -> Session value () 
removeParticipant location participant = 
    State.modify $ \state@ExecutionState { locations } -> 
        state 
            { locations = Map.update (Just . Map.delete participant) location locations
            }

removeLocation :: Location -> Session value () 
removeLocation location = 
    State.modify $ \state@ExecutionState { locations } -> 
        state 
            { locations = Map.delete location locations
            }



lookupVariable :: Participant -> Identifier -> Session value value
lookupVariable participant identifier = do 
    monitor <- lookupParticipant participant 
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


data Error 
    = SessionNotInSync 
    | UndefinedParticipant Participant
    | UndefinedVariable Participant Identifier
    | EmptyQueue
    | EmptyQueueHistory
    | InvalidQueueItem
    | InvalidBackwardQueueItem 
 
forward :: Location -> Participant -> Program Value -> Monitor Value String -> Session Value ()  
forward location participant program monitor = 
    let (previous, fixedLocal) = _localType monitor
        localType = unFix fixedLocal
    in
    case ( unFix program, localType) of 
        (Send payload continuation, LocalType.Send target tipe continuationType) -> do
            value <- evaluateValue participant payload 
            let newLocalTypeState = 
                    ( Fix $ LocalType.BackwardSend target tipe previous 
                    , continuationType
                    ) 
            push ( participant, target, value )
            setParticipant location participant (monitor { _localType = newLocalTypeState }, continuation)

        (Receive continuation, LocalType.Receive sender tipe continuationType) -> do
            ( s, r, payload ) <- pop
            variableName <- uniqueVariableName
            if s == sender && r == participant then do
                let newBindings = Map.insert variableName payload $ _store monitor 
                    newLocalTypeState = 
                        ( Fix $ LocalType.BackwardReceive sender tipe previous 
                        , continuationType
                        ) 
                    newMonitor = monitor { _store = newBindings, _localType = newLocalTypeState }  
                    
                setParticipant location participant (newMonitor, continuation)
            else
                Except.throwError InvalidQueueItem
        
        (Application functionName argumentName, _) -> do
            functionValue <- lookupVariable participant functionName 
            argumentValue <- lookupVariable participant argumentName

            convertToFunction <- isFunction <$> State.get
            case convertToFunction functionValue of
                Nothing -> error "function is not defined" 
                Just (variable, body) -> do
                    k <- uniqueApplicationName
                    let newLocalType = (Fix $ LocalType.Application k previous, fixedLocal) 
                        newMonitor = 
                            monitor 
                                { _store = Map.insert variable argumentValue (_store monitor)
                                , _localType = newLocalType
                                , _applicationHistory = Map.insert k (functionName, argumentName) (_applicationHistory monitor)
                                }  
                    setParticipant location participant (newMonitor, body)

        (Parallel p q, _) -> do
            l1 <- uniqueLocation 
            l2 <- uniqueLocation 

            let newLocations = 
                    Map.empty
                        |> Map.insert l1 p
                        |> Map.insert l2 q
                        |> Map.insert location (Fix NoOp) 

            let ( previous, next ) = _localType monitor 
                newMonitor = monitor { _localType = ( Fix $ LocalType.Spawning location l1 l2 previous, next ) } 

            setParticipant location participant ( newMonitor, Fix NoOp )
            setParticipant l1 participant ( newMonitor, p )
            setParticipant l2 participant ( newMonitor, q )

        (Let visibleName value continuation, _) -> do
            variableName <- uniqueVariableName 

            let newLocalType = (Fix $ LocalType.Assignment visibleName variableName previous, fixedLocal) 
                newMonitor = 
                    monitor 
                        { _store = Map.insert variableName value (_store monitor)
                        , _localType = newLocalType
                        }  
            
            setParticipant location participant ( newMonitor, renameVariable visibleName variableName continuation )

        (Literal lit, _) -> do
            variableName <- uniqueVariableName 

            let newLocalType = (Fix $ LocalType.Literal variableName previous, fixedLocal) 
                newMonitor = 
                    monitor 
                        { _store = Map.insert variableName lit (_store monitor)
                        , _localType = newLocalType
                        }  
            
            setParticipant location participant ( newMonitor, Fix NoOp )

        (Send _ _, _) ->  
            Except.throwError SessionNotInSync

        (Receive _, _) ->  
            Except.throwError SessionNotInSync

        ( NoOp, _ ) -> 
            return () 

checkSynchronized :: Participant -> Participant -> Session value ()
checkSynchronized sender receiver = return () 


backward :: Location -> Participant -> Program value -> Monitor value String -> Session value ()  
backward location participant program monitor = 
    let (previous, next) = _localType monitor 
        setParticipant_ l p (m, progF) = setParticipant l p (m, Fix progF)
    in
    case unFix previous of 
        LocalType.BackwardSend receiver tipe rest -> do
            -- pop from the future! (not history; rolling the receive will put the action into the future)
            (source, target, payload ) <- pop
            let newLocalType = ( rest, Fix $ LocalType.Send receiver tipe next )
            if source == participant && target == receiver then do
                -- check whether the session types line up, before rolling
                checkSynchronized source target 

                setParticipant_ location participant 
                    ( monitor { _localType = newLocalType } 
                    , Semantics.Send { value = payload, continuation = program } 
                    )
            else 
                Except.throwError InvalidBackwardQueueItem 
    
        LocalType.BackwardReceive sender tipe rest -> do 
            ( source, target, payload ) <- popHistory 
            let newLocalType = ( rest, Fix $ LocalType.Receive sender tipe next ) 
            if target == participant && source == sender then do
                -- check whether the session types line up, before rolling
                checkSynchronized source target 

                setParticipant_ location participant 
                    ( monitor { _localType = newLocalType } 
                    , Semantics.Receive { continuation = program } 
                    )
            else 
                Except.throwError InvalidBackwardQueueItem 

        LocalType.SendOrReceive _ _ -> 
            error "satisfy the exhaustiveness checker"

        LocalType.Application k rest ->
            case Map.lookup k (_applicationHistory monitor) of 
                Nothing -> 
                    error "rolling function that does not exist"
                Just ( function, argument ) -> 
                    setParticipant_ location participant 
                        ( monitor { _localType = ( rest, next ), _applicationHistory = Map.delete k (_applicationHistory monitor) } 
                        , Semantics.Application function argument
                        )


        LocalType.Spawning l l1 l2 rest | l /= location -> 
            error "rolling someone else's spawn"

        LocalType.Spawning l l1 l2 rest -> do
            (_, p1) <- getParticipant l1 participant
            (_, p2) <- getParticipant l1 participant

            case unFix program of 
                NoOp -> do
                    let newLocalType = ( rest, next )
                    removeLocation l1
                    removeLocation l2

                    setParticipant_ location participant ( monitor { _localType = newLocalType }, Semantics.Parallel p1 p2 )

                _ -> 
                    error "rolling a program that is not NoOp"

        LocalType.Assignment visibleName variableName rest -> do
            let 
                store = _store monitor 

                value = 
                    Map.lookup variableName store
                        |> fromMaybe (error "undoing assignment to variable that does not exist")

                newMonitor = 
                    monitor 
                        { _store = Map.delete variableName store
                        , _localType = (rest, next)
                        }  
            
            setParticipant_ location participant 
                ( newMonitor
                , Semantics.Let visibleName  value program
                )

        LocalType.Literal variableName rest -> do
            let 
                store = _store monitor 

                value = 
                    Map.lookup variableName store
                        |> fromMaybe (error "undoing literal that does not exist")

                newMonitor = 
                    monitor 
                        { _store = Map.delete variableName store
                        , _localType = (rest, next)
                        }  
            
            setParticipant_ location participant 
                ( newMonitor
                , Semantics.Literal value 
                )

        LocalType.Hole -> 
            return () 

data ProgramF value f 
    = Send { value :: value, continuation :: f }
    | Receive { continuation :: f  }
    | Parallel f f 
    | Application Identifier Identifier
    | Let Identifier value f 
    | Literal value -- needed to define multi-parameter functions
    | NoOp
    deriving (Generic, Functor, Foldable, Traversable, ToJSON, FromJSON)


f2 :: Program Value
f2 = Fix $ Literal $ VFunction "x" $ Fix $ Literal $ VFunction "y" $ Fix NoOp

type Program value = Fix (ProgramF value) 

deriving instance (ToJSON (f (Fix f))) => ToJSON (Fix f)
deriving instance (FromJSON (f (Fix f))) => FromJSON (Fix f)
deriving instance (ElmType (f (Fix f))) => ElmType (Fix f)

-- newtype P v = P (Program v) deriving (Generic, ToJSON, FromJSON)


data Value 
    = VBool Bool
    | VInt Int
    | VString String
    | VEnd 
    | VFunction Identifier (Program Value)
    | VReference Identifier 

renameVariable :: Identifier -> Identifier -> Program Value -> Program Value
renameVariable old new program = 
    let rename v = 
            if v == old then 
                new 
            else    
                v

        helper instruction = 
            case instruction of 
                Application functionName variableName ->
                    Application (rename functionName) variableName  

                Let variableName value continuation ->  
                    let newValue = 
                            case value of 
                                VReference identifier -> 
                                    VReference (rename identifier)

                                _ -> 
                                    value
                    in
                        Let variableName newValue continuation

                _ -> 
                    instruction
    in
        Fix.cata (Fix . helper) program 

{-| Evaluate a value until a base type, by dereferencing references -}
evaluateValue :: Participant -> Value -> Session Value Value 
evaluateValue participant value = 
    case value of 
        VReference identifier -> do 
            store <- _store <$> lookupParticipant participant
            case Map.lookup identifier store of
                Nothing -> error "reference value to undefined variable"
                Just v -> 
                    evaluateValue participant v
            
        _ -> 
            return value

data Monitor value tipe = 
    Monitor 
        { _localType :: LocalTypeState tipe
        , _freeVariables :: List Identifier
        , _store :: Map Identifier value 
        , _applicationHistory :: Map Identifier (Identifier, Identifier)
        , _reversible :: Bool
        }


data Queue a = Queue { history :: List ( Participant, Participant, a), current :: List (Participant, Participant, a) } 

push :: (Participant, Participant, value) -> Session value ()
push item = 
    State.modify $ \state@ExecutionState { queue } -> 
        state { queue = enqueue item queue }

pop :: Session value (Participant, Participant, value)
pop = do
    q <- queue <$> State.get
    case dequeue q of 
        Nothing -> Except.throwError EmptyQueue
        Just (value, newQueue ) -> do 
            State.modify $ \state@ExecutionState { queue } -> 
                state { queue = newQueue }
            return value


popHistory :: Session value (Participant, Participant, value)
popHistory = do
    q <- queue <$> State.get
    case dequeueHistory q of 
        Nothing -> Except.throwError EmptyQueueHistory
        Just (value, newQueue ) -> do 
            State.modify $ \state@ExecutionState { queue } -> 
                state { queue = newQueue }
            return value

enqueue :: ( Participant, Participant, a ) -> Queue a -> Queue a
enqueue item (queue@Queue{ current }) = queue { current = current ++ [ item ] }

dequeue :: Queue a -> Maybe ((Participant, Participant, a), Queue a)
dequeue queue@Queue{ history, current } = 
    case current of 
        [] -> Nothing
        (x:xs) -> 
            Just ( x, queue { current = xs, history = x : history } )

dequeueHistory :: Queue a -> Maybe ((Participant, Participant, a), Queue a)
dequeueHistory queue@Queue{ history, current } = 
    case history of 
        [] -> Nothing
        (x:xs) -> 
            Just ( x, queue { current = x : current, history = xs } )


