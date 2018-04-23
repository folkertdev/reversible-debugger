{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, DuplicateRecordFields, NamedFieldPuns, DeriveAnyClass, StandaloneDeriving, UndecidableInstances  #-}
module Semantics where 

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Arrow (first)

import LocalType (LocalType, LocalTypeState, TypeContextF(Selected, Offered), Location, Participant, Identifier, Choice(..))
import qualified LocalType
import qualified GlobalType

import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.Fix as Fix
import Data.List as List
import Data.Maybe as Maybe

import Types ((|>))
import qualified Zipper

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)

import Debug.Trace as Debug

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

instance Show v => Show (ExecutionState v) where 
    show state = 
        "locations: " ++ show (locations state) 
            ++ "\n\n\n" 
            ++ "participants: " ++ show (participants state)
            ++ "\n\n\n" 
            ++ "queue: " ++ show (queue state)

instance Eq v => Eq (ExecutionState v) where 
    a == b = locations a == locations b && participants a == participants b && queue a == queue b

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
    | InvalidBackwardQueueItem String
    deriving (Eq, Show)


ensure condition error = 
    if condition then 
        return () 
    else 
        Except.throwError error
 
forward :: Location -> Participant -> Program Value -> Monitor Value String -> Session Value ()  
forward location participant program monitor = 
    let (previous, fixedLocal) = _localType monitor
        localType = unFix fixedLocal
    in
    case ( unFix program, localType) of 
        ( _, LocalType.RecursionPoint rest ) ->
            let 
                newLocalType = (Fix $ LocalType.BackwardRecursionPoint previous, rest) 
                newMonitor = 
                    monitor 
                        { _recursionPoints = rest : _recursionPoints monitor 
                        , _localType = newLocalType
                        }  
                
            in do
                setParticipant location participant ( newMonitor, program ) 
                forward location participant program newMonitor

        ( _, LocalType.WeakenRecursion rest ) ->
            let 
                newLocalType = (Fix $ LocalType.BackwardWeakenRecursion previous, rest) 
                newMonitor = 
                    monitor 
                        { _recursiveVariableNumber = 1 + _recursiveVariableNumber monitor 
                        , _localType = newLocalType
                        }  
                
            in do
                setParticipant location participant ( newMonitor, program ) 
                forward location participant program newMonitor

        ( _, LocalType.RecursionVariable ) ->
            let 
                continuationType = 
                    case drop (_recursiveVariableNumber monitor) (_recursionPoints monitor) of 
                        [] -> error "recursion to nothing"
                        x:xs -> 
                            x

                newLocalType = (Fix $ LocalType.BackwardRecursionVariable previous, continuationType) 
                -- newLocalType = (previous, continuationType) 
                newMonitor = 
                    monitor 
                        { _localType = newLocalType
                            , _recursionPoints = take (_recursiveVariableNumber monitor + 1) (_recursionPoints monitor) 
                        }  
                
            in do
                setParticipant location participant ( newMonitor, program ) 
                forward location participant program newMonitor

        (Send owner payload continuation, _) ->
            checkAndPerformSend location participant monitor owner payload continuation 

        (Receive owner visibleName continuation, _) ->
            checkAndPerformReceive location participant monitor owner visibleName continuation 

        (Select owner options, _) -> 
            case localType of 
                LocalType.Select expectedOwner offerer types -> do
                    ensure (owner == expectedOwner) (error $ "Select owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)

                    let f :: (String, Value, Program Value, c) -> Session Value (String, Bool, Value, Program Value, c) 
                        f (label, value, program, c) = do
                            evaluated <- evaluateValue participant value
                            return (label, unsafeCastToBool evaluated, value, program, c) 
                    
                        options_ = -- List.zipWith (\(a,b) c -> (a,b,c)) options types
                            options
                                |> List.map (\(l, v, p) -> (\t -> (l, v,p,t)) <$> Map.lookup l types)
                                |> Maybe.catMaybes

                    evaluated :: List (String, Bool, Value, Program Value, LocalType String) <- mapM f options_
                    case break (\(_, condition, _, _,_) -> condition) evaluated of 
                        (notTaken, []) -> 
                            -- there are no valid options, now what?
                            undefined

                        (notTaken, taken@(takenLabel, _, takenCondition, takenProgram, takenType):alsoNotTaken) -> 
                            -- v is likely broken, fix later

                            let 
                                f (l, a,b,c,d) = (l, b,c,d)
                                selection = Zipper.fromSegments (f <$> notTaken) (f taken) (f <$> alsoNotTaken)
                                newLocalType = (LocalType.backwardSelect owner offerer selection previous, takenType)
                                newMonitor = 
                                    monitor { _localType = newLocalType }  
                            in do
                                push ( offerer, participant, VLabel takenLabel )
                                setParticipant location participant ( newMonitor, takenProgram ) 

                _ -> 
                    error $ "Select needs a LocalType.Select, but got " ++ show localType

        (Offer owner options, _) -> 
            case localType of 
                LocalType.Offer expectedOwner selector types -> do
                    ensure (owner == expectedOwner) (error $ "Offer: owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)

                    ( s, r, VLabel label ) <- pop

                    case (,) <$> Map.lookup label (Map.fromList options) <*> Map.lookup label types of  
                        Nothing ->
                            -- there are no valid options, now what?
                            undefined

                        Just (program, tipe) -> 
                            let 
                                combined = 
                                    options
                                        |> List.map (\(l, p) -> (\t -> (l, p, t)) <$> Map.lookup l types)
                                        |> Maybe.catMaybes

                                notChosen (l, _,_) = l /= label
                                selection = Zipper.fromSegments (takeWhile notChosen combined) (label, program, tipe) (drop 1 $ dropWhile notChosen combined)

                                newLocalType = 
                                    (LocalType.backwardOffer owner selector selection previous, tipe)

                                newMonitor = 
                                    monitor { _localType = newLocalType }  
                            in 
                                setParticipant location participant ( newMonitor, program ) 

                _ -> 
                    error $ "Offer needs a LocalType.Offer, but got " ++ show localType
        
        (Application functionName argument, _) -> do
            functionValue <- lookupVariable participant functionName 
            argumentValue <- evaluateValue participant argument 
            variableName <- uniqueVariableName 

            convertToFunction <- isFunction <$> State.get
            case convertToFunction functionValue of
                Nothing -> error "function is not defined" 
                Just (variable, body) -> do
                    k <- uniqueApplicationName
                    let newLocalType = (Fix $ LocalType.Application variableName k previous, fixedLocal) 
                        newMonitor = 
                            monitor 
                                { _store = Map.insert variableName argumentValue (_store monitor)
                                , _localType = newLocalType
                                , _applicationHistory = Map.insert k (functionName, argument) (_applicationHistory monitor)
                                }  
                    setParticipant location participant (newMonitor, renameVariable variable variableName body)

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
                        { _store = Map.insert variableName (renameValue visibleName variableName value) (_store monitor)
                        , _localType = newLocalType
                        }  
            
            setParticipant location participant ( newMonitor, renameVariable visibleName variableName continuation )

        (Literal lit, _) -> do
            variableName <- uniqueVariableName 

            {-
            let  
                newLocalType = (Fix $ LocalType.Literal variableName previous, fixedLocal) 
                newMonitor = 
                    monitor 
                        { _store = Map.insert variableName lit (_store monitor)
                        , _localType = newLocalType
                        }  
            -}
            
            setParticipant location participant ( monitor, Fix NoOp )

        ( NoOp, _ ) -> 
            return () 



checkAndPerformSend :: Location -> Participant -> Monitor Value String -> Participant -> Value -> Program Value -> Session Value ()
checkAndPerformSend location participant monitor owner payload continuation = 
    if participant == owner then 
        let (previous, fixedLocal) = _localType monitor
            localType = unFix fixedLocal
        in
        case localType of 
            LocalType.Send expectedOwner target tipe continuationType -> do
                ensure (owner == expectedOwner) (error $ "Send owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)
                value <- evaluateValue participant payload 
                let newLocalTypeState = 
                        ( Fix $ LocalType.BackwardSend owner target tipe previous 
                        , continuationType
                        ) 
                push ( participant, target, value )
                setParticipant location participant (monitor { _localType = newLocalTypeState }, continuation)

            _ -> 
                error $ "`" ++ participant ++ "`'s program wants to perform a send, but its type is " ++ show localType
    else do
        state <- State.get
        case Map.lookup owner (participants state) of 
            Nothing -> 
                error "owner does not have a monitor"
            Just ownerMonitor -> do
                forward location owner (Fix $ Send owner payload continuation) ownerMonitor
                setParticipant location participant (monitor, continuation)


checkAndPerformReceive :: Location -> Participant -> Monitor Value String -> Participant -> Identifier -> Program Value -> Session Value ()
checkAndPerformReceive location participant monitor owner visibleName continuation = 
    if participant == owner then 
        let (previous, fixedLocal) = _localType monitor
            localType = unFix fixedLocal
        in
        case localType of 
            LocalType.Receive expectedOwner sender tipe continuationType -> do
                ensure (owner == expectedOwner) (error $ "Receive owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)
                ( s, r, payload ) <- pop
                variableName <- uniqueVariableName
                if s == sender && r == participant then do
                    let newBindings = Map.insert variableName payload $ _store monitor 
                        newLocalTypeState = 
                            ( Fix $ LocalType.BackwardReceive owner sender visibleName variableName tipe previous 
                            , continuationType
                            ) 
                        newMonitor = monitor { _store = newBindings, _localType = newLocalTypeState }  
                        
                    setParticipant location participant (newMonitor, renameVariable visibleName variableName continuation )
                else
                    let message = 
                            "Receive: I encountered a mismatch looking at participant `" ++ participant ++ "`: \n\n" 
                                ++ if s /= sender then 
                                      "The type expects the sender to be `" ++ sender ++ "`, but the queue has a message with sender `" ++ s ++ "`\n" 
                                   else ""
                                ++ if r /= participant then 
                                       "The type expects the receiver to be the current participant `" ++ participant ++ "`, but the queue has a message with receiver `" ++ r ++ "`\n" 
                                   else ""
                    in 
                    Except.throwError InvalidQueueItem
                        |> Debug.traceShow message 

            _ -> 
                error $ "`" ++ participant ++ "`'s program wants to perform a receive, but its type is " ++ show localType
    else do
        state <- State.get
        case Map.lookup owner (participants state) of 
            Nothing -> 
                error "owner does not have a monitor"
            Just ownerMonitor -> do
                forward location owner (Fix $ Receive owner visibleName continuation) ownerMonitor
                setParticipant location participant (monitor, continuation )



checkSynchronized :: Participant -> Participant -> Session value ()
checkSynchronized sender receiver = return ()  -- TODO


checkSynchronizedForChoice :: Participant -> Participant -> Session value ()
checkSynchronizedForChoice offerer selector = return ()  -- TODO


backward :: Show value => Location -> Participant -> Program value -> Monitor value String -> Session value ()  
backward location participant program monitor = 
    let (previous, next) = _localType monitor 
        setParticipant_ l p (m, progF) = setParticipant l p (m, Fix progF)
    in
    case unFix previous of 
        LocalType.BackwardSend owner receiver tipe rest -> do
            -- pop from the future! (not history; rolling the receive will put the action into the future)
            (source, target, payload ) <- pop
            let newLocalType = ( rest, Fix $ LocalType.Send owner receiver tipe next )
            if source == participant && target == receiver then do
                -- check whether the session types line up, before rolling
                checkSynchronized source target 

                -- remove this send/receive combo from the queue
                queueRemoveHistory

                setParticipant_ location participant 
                    ( monitor { _localType = newLocalType } 
                    , Semantics.Send { owner = owner, value = payload, continuation = program } 
                    )
            else 
                Except.throwError $ InvalidBackwardQueueItem "in BackwardSend"
    
        LocalType.BackwardReceive owner sender visibleName variableName tipe rest -> do 
            ( source, target, payload ) <- popHistory 
            let newLocalType = ( rest, Fix $ LocalType.Receive owner sender tipe next ) 
            if target == participant && source == sender then do
                -- check whether the session types line up, before rolling
                checkSynchronized source target 

                setParticipant_ location participant 
                    ( monitor { _localType = newLocalType, _store = Map.delete variableName (_store monitor) } 
                    , Semantics.Receive { owner = owner, variableName = visibleName, continuation = program } 
                    )
            else 
                Except.throwError $ InvalidBackwardQueueItem "in BackwardReceive"


        LocalType.Selected { owner, offerer, selection, continuation } -> do
            -- pop from the future! (not history; rolling the receive will put the action into the future)
            (expectedOfferer, expectedSelector, _ ) <- pop
            if participant == owner && offerer == expectedOfferer && owner == expectedSelector then do 
                checkSynchronizedForChoice offerer owner 
                let 
                    combined = Zipper.toList selection 
                    types = List.map (\(l, _,_,t) -> (l, t)) combined 
                    options = List.map (\(label, condition, program, _) -> (label, condition, program)) combined

                -- remove this offer/select combo from the queue
                queueRemoveHistory

                setParticipant_ location participant 
                    ( monitor { _localType = ( continuation,  LocalType.select owner offerer types )}
                    , Semantics.Select owner options 
                    )

            else 
                Except.throwError $ InvalidBackwardQueueItem "in Selected" 

        LocalType.Offered { owner, selector, picked, continuation } -> do
            ( expectedOfferer, expectedSelector, _ ) <- popHistory 
            if owner == participant && owner == expectedOfferer && selector == expectedSelector then do
                -- check whether the session types line up, before rolling
                checkSynchronizedForChoice owner selector 
                let 
                    combined = Zipper.toList picked 
                    (programs, types) =
                       combined
                            |> List.map (\(label, program, tipe) -> ((label, program), (label, tipe)))
                            |> List.unzip

                setParticipant_ location participant 
                    ( monitor { _localType = ( continuation,  LocalType.offer owner selector types )}
                    , Semantics.Offer owner programs 
                    )

            else 
                Except.throwError $ InvalidBackwardQueueItem $ "in Offered" ++ show ((owner, participant), (owner, expectedOfferer) , (selector, expectedSelector))

        LocalType.Application variableName k rest ->
            case Map.lookup k (_applicationHistory monitor) of 
                Nothing -> 
                    error "rolling function that does not exist"
                Just ( function, argument ) -> 
                    setParticipant_ location participant 
                        ( monitor 
                            { _localType = ( rest, next )
                            , _store = Map.delete variableName (_store monitor)
                            , _applicationHistory = Map.delete k (_applicationHistory monitor) 
                            } 
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

        LocalType.Assignment visibleName variableName rest -> 
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
            in            
                setParticipant_ location participant 
                    ( newMonitor
                    , Semantics.Let visibleName  value program
                    )

        LocalType.Literal variableName rest -> 
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
            in            
                setParticipant_ location participant 
                    ( newMonitor
                    , Semantics.Literal value 
                    )

        LocalType.BackwardRecursionPoint rest -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = (rest, LocalType.recurse next)
                        }  
            in
                setParticipant_ location participant 
                    ( newMonitor
                    , unFix program 
                    )

        LocalType.BackwardWeakenRecursion rest -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = (rest, LocalType.broadenScope next)
                        }  
            in
                setParticipant_ location participant 
                    ( newMonitor
                    , unFix program 
                    )

        LocalType.BackwardRecursionVariable rest -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = (rest, LocalType.recursionVariable)
                        }  
            in
                setParticipant_ location participant 
                    ( newMonitor
                    , unFix program 
                    )

        LocalType.Hole -> 
            return () 

        LocalType.SendOrReceive _ _ -> 
            error $ "satisfy the exhaustiveness checker" ++ show (unFix previous)

data ProgramF value f 
    = Send { owner :: Participant, value :: value, continuation :: f }
    | Receive { owner :: Participant, variableName :: Identifier, continuation :: f  }
    | Offer Participant (List (String, f))
    | Select Participant (List (String, value, f))
    | Parallel f f 
    | Application Identifier value
    | Let Identifier value f 
    -- | IfThenElse value f f
    | Literal value -- needed to define multi-parameter functions
    | NoOp
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable, ToJSON, FromJSON, ElmType)

deriving instance (ElmType a, ElmType b, ElmType c) => ElmType (a,b,c)

run :: List (Program value -> Program value) -> Program value
run = foldr ($) terminate


parallel :: List (Program value) -> Program value
parallel = foldr1 (\a b -> Fix (Parallel a b))

send :: Participant -> value -> Program value -> Program value
send o v c = Fix (Send o v c)

receive :: Participant -> Identifier -> Program value -> Program value
receive o v c = Fix (Receive o v c)

terminate :: Program value
terminate = Fix NoOp 

applyFunction :: Identifier -> value -> Program value
applyFunction functionName argument = Fix $ Application functionName argument

literal :: value -> Program value
literal = Fix . Literal

-- ifThenElse :: value -> Program value -> Program value -> Program value
-- ifThenElse condition thenBranch elseBranch = Fix $ IfThenElse condition thenBranch elseBranch

select :: Participant -> List (String, value, Program value) -> Program value
select owner options = Fix $ Select owner options

offer :: Participant -> List (String, Program value) -> Program value
offer owner options = Fix $ Offer owner options

letBinding :: Identifier -> value -> Program value -> Program value
letBinding name value cont = Fix $ Let name value cont


forward_ :: Location -> Participant -> Session Value ()        
forward_ location participant = do
    state <- State.get
    case Map.lookup participant (participants state) of 
        Nothing -> 
            error "unknown participant"
        Just monitor -> 
            case Map.lookup location (locations state) >>= Map.lookup participant of 
                Just program -> 
                    forward location participant program monitor
                Nothing -> 
                    error $ "location has no participant named " ++ participant


backward_ :: Location -> Participant -> Session Value ()        
backward_ location participant = do
    state <- State.get
    case Map.lookup participant (participants state) of 
        Nothing -> 
            error "unknown participant"
        Just monitor -> 
            case Map.lookup location (locations state) >>= Map.lookup participant of 
                Just program -> 
                    backward location participant program monitor
                Nothing -> 
                    error $ "location has no participant named " ++ participant

forwardTestable :: Location -> Participant -> ExecutionState Value -> Either Error (ExecutionState Value)
forwardTestable location participant state = 
    snd <$> State.runStateT (forward_ location participant) state

backwardTestable :: Location -> Participant -> ExecutionState Value -> Either Error (ExecutionState Value)
backwardTestable location participant state = 
    snd <$> State.runStateT (backward_ location participant) state



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
    | VIntOperator Value IntOperator Value 
    | VComparison Value Ordering Value
    | VUnit
    | VFunction Identifier (Program Value)
    | VReference Identifier 
    | VLabel String
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

deriving instance ElmType Ordering

unsafeCastToBool value = 
    case value of 
        VBool b -> b
        _ -> error "value is not a bool"

renameValue :: Identifier -> Identifier -> Value -> Value
renameValue old new value = 
    case value of 
        VReference current -> 
            if current == old then VReference new else VReference current

        VIntOperator a op b -> 
            VIntOperator (renameValue old new a) op (renameValue old new b) 

        VComparison a op b -> 
            VComparison (renameValue old new a) op (renameValue old new b) 

        VFunction argumentName program -> 
            if old == argumentName then
                -- variable shadowing
                VFunction argumentName program

            else
                VFunction argumentName (renameVariable old new program)

        _ -> 
            value

data IntOperator 
    = Add
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

instance Ord Value where
    compare value1 value2 = 
        case (value1, value2) of 
            (VInt a, VInt b)  -> Prelude.compare a b
            (VBool a, VBool b)  -> Prelude.compare a b
            (VString a, VString b)  -> Prelude.compare a b
            (VUnit, VUnit)  -> Prelude.compare () () 
            (VFunction _ _, VFunction _ _)  -> error "cannot compare function values"
            (VReference a, VReference b)  -> Prelude.compare a b 
            _ -> error $ "conflicting value types " ++ show (value1, value2)
    


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
                    Application (rename functionName) (renameValue old new variableName)

                Let variableName value continuation ->  
                    Let variableName (renameValue old new value) continuation

                Select owner options -> 
                    Select owner $ List.map (\(label, condition, program) -> (label, renameValue old new condition, program)) options

                Send owner variable cont ->
                    Send owner (renameValue old new variable) cont

                Literal literal  ->
                    Literal (renameValue old new literal)

                _ -> 
                    instruction
    in
        Fix.cata (Fix . helper) program 


chaseReference :: Participant -> Value -> Session Value Value 
chaseReference participant value = 
    case value of 
        VReference identifier -> do 
            store <- _store <$> lookupParticipant participant
            case Map.lookup identifier store of
                Nothing -> 
                    let message = 
                            "The variable `" ++ identifier ++ "` is undefined for participant `" ++ participant ++ "`"
                    in error message 
                Just v -> 
                    chaseReference participant v
        
        VIntOperator a operator b -> do
            x <- chaseReference participant a
            y <- chaseReference participant b
            pure $ VIntOperator x operator y 


        VComparison a expected b -> do
            x <- chaseReference participant a
            y <- chaseReference participant b
            pure $ VComparison a expected b

        _ -> 
            pure value

{-| Evaluate a value until a base type, by dereferencing references -}
evaluateValue :: Participant -> Value -> Session Value Value 
evaluateValue participant value = 
    case value of 
        VReference identifier -> do 
            store <- _store <$> lookupParticipant participant
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

decrement :: Participant -> Value -> Session Value Value 
decrement participant value_ = do
    value <- evaluateValue participant value_
    case value of 
        VInt num -> pure $ VInt (num - 1)
        _ -> error $ "type error in value, decrement on " ++ show value


data Monitor value tipe = 
    Monitor 
        { _localType :: LocalTypeState (Program value) value tipe
        , _recursiveVariableNumber :: Int
        , _recursionPoints :: List (LocalType tipe)
        , _freeVariables :: List Identifier
        , _store :: Map Identifier value 
        , _applicationHistory :: Map Identifier (Identifier, value)
        , _reversible :: Bool
        }
        deriving (Show, Eq)


data Queue a = Queue { history :: List ( Participant, Participant, a), current :: List (Participant, Participant, a) }  deriving (Eq, Show)

emptyQueue = Queue [] [] 

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


queueRemoveHistory :: Session value () 
queueRemoveHistory = do 
    queue@Queue{ history, current } <- queue <$> State.get
    case history of 
        [] -> 
            Except.throwError EmptyQueueHistory

        (x:xs) -> do
            let newQueue =  queue { history = xs }
            State.modify $ \state@ExecutionState { queue } -> 
                state { queue = newQueue }
            return ()


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

enqueueHistory :: ( Participant, Participant, a ) -> Queue a -> Queue a
enqueueHistory item (queue@Queue{ history }) = queue { history = item : history }

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

atIndex :: Int -> List a -> Maybe a
atIndex 0 (x:xs) = Just x
atIndex n (x:xs) = atIndex (n - 1) xs
atIndex _ [] = Nothing 
        

{-
x = 
    fromList [("A",
        Monitor 
            { _localType = (Fix (Transaction (TSend {owner = "A", receiver = "B", tipe = "number", continuation = Fix (Choice (COffer {owner = "A", selector = "B", options = fromList [("end",Fix (Atom End)),("recurse",Fix (Atom V))]}))})))
            , _recursiveVariableNumber = 0
            , _recursionPoints = [Fix (Transaction (TSend {owner = "A", receiver = "B", tipe = "number", continuation = Fix (Choice (COffer {owner = "A", selector = "B", options = fromList [("end",Fix (Atom End)),("recurse",Fix (Atom V))]}))}))]
            , _freeVariables = []
            , _store = fromList [("v1",VInt 1),("v5",VInt 0)]
            , _applicationHistory = fromList [("k0",("v0",VInt 1))], _reversible = False})
             ]
-}
