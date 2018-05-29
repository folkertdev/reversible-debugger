{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Semantics where 

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Arrow (first, second)
import Control.Monad.Free as Free

import LocalType (LocalType, LocalTypeState, TypeContextF(Selected, Offered), Location, Participant, Identifier, Choice(..))
import qualified LocalType
import qualified GlobalType

import Data.Maybe (fromMaybe)
import Data.Map as Map (Map)
import qualified Data.Set as Set 
import qualified Data.Map as Map
import Data.Fix as Fix
import Data.List as List
import Data.Maybe as Maybe

import Utils ((|>), List)
import Utils.Maybe as Maybe
import qualified Zipper
import Queue (Queue, QueueError)
import qualified Queue

import Debug.Trace as Debug

import Program 
import Session
import Synchronization (checkSynchronizedForTransaction, checkSynchronizedForChoice)


validateLocalType :: Location -> Participant -> Monitor Value String -> Session Value (LocalType.TypeContext String, LocalType String)
validateLocalType location participant monitor = 
    case _localType monitor of 
        LocalType.Unsynchronized x -> 
            return x 

        LocalType.Synchronized _ -> 
            Except.throwError $ SynchronizationError $ "cannot move forward a synchronized local type: it has to take a step back first. For participant " ++ participant ++ " at location " ++ location


forward :: Location -> Session Value ()
forward location = do
    -- get the program
    ExecutionState { locations, participants } <- State.get

    case Map.lookup location locations of 
        Nothing -> 
            error $ "unknown location " ++ show location 

        Just (defaultParticipant, otherOptionsStack, program) -> do
            -- get the owner/participant
            let owner = statementOwner (unFix program) |> Maybe.withDefault defaultParticipant
            case Map.lookup owner participants of 
                Nothing -> 
                    error $ "unknown participant " ++ show owner

                Just monitor -> do
                    ( historyType, futureType ) <- validateLocalType location owner monitor
                    forwardHelper location owner monitor (historyType, futureType) program otherOptionsStack

            
forwardHelper location owner monitor (historyType, futureType) program otherOptionsStack = do
    let setParticipantDefault location owner ( newMonitor, newProgram ) = 
            setParticipant location owner ( newMonitor, otherOptionsStack, newProgram )

        setParticipantWithNewStack = setParticipant 

    case ( unFix program, unFix futureType ) of 
        ( _, LocalType.RecursionPoint rest ) ->
            let 
                newLocalType = LocalType.createState (Fix $ LocalType.BackwardRecursionPoint historyType) rest
                newMonitor = 
                    monitor 
                        { _recursionPoints = rest : _recursionPoints monitor 
                        , _localType = newLocalType
                        }  
                
            in do
                setParticipantDefault location owner ( newMonitor, program ) 
                forward location 

        ( _, LocalType.WeakenRecursion rest ) ->
            let 
                newLocalType = LocalType.createState (Fix $ LocalType.BackwardWeakenRecursion historyType) rest 
                newMonitor = 
                    monitor 
                        { _recursiveVariableNumber = 1 + _recursiveVariableNumber monitor 
                        , _localType = newLocalType
                        }  
                
            in do
                setParticipantDefault location owner ( newMonitor, program ) 
                forward location 

        ( _, LocalType.RecursionVariable ) ->
            let 
                index = _recursiveVariableNumber monitor
                options = _recursionPoints monitor
                continuationType = 
                    case drop index options of 
                        [] -> error $ "recursion to nothing, looking for index " ++ show index ++ " in " ++ show options
                        x:xs -> 
                            x

                newLocalType = LocalType.createState (Fix $ LocalType.BackwardRecursionVariable historyType) continuationType
                newMonitor = 
                    monitor 
                        { _localType = newLocalType
                        -- , _recursionPoints = take (_recursiveVariableNumber monitor + 1) (_recursionPoints monitor) 
                        }  
                
            in do
                setParticipantDefault location owner ( newMonitor, program ) 
                forward location 

        (Send owner payload continuation, _) -> 
            setParticipantDefault location owner =<< checkAndPerformSend location owner monitor owner payload continuation 

        (Receive owner visibleName continuation, _) ->
            setParticipantDefault location owner =<< checkAndPerformReceive location owner monitor owner visibleName continuation 

        (Select owner options, _) -> 
            case unFix futureType of 
                LocalType.Select expectedOwner offerer types -> do
                    ensure (owner == expectedOwner) (error $ "Select owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)

                    let f :: (String, Value, Program Value, c) -> Session Value (String, Bool, Value, Program Value, c) 
                        f (label, value, program, c) = do
                            evaluated <- evaluateValue owner value
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

                                processSelection = Zipper.map (\(label, value, process, tipe) -> (label, value, process)) selection
                                typeSelection = Zipper.map (\(label, value, process, tipe) -> (label, tipe)) selection 

                                newLocalType = LocalType.createState (LocalType.backwardSelect owner offerer typeSelection historyType) takenType

                                newMonitor = 
                                    monitor { _localType = newLocalType }  
                            in do
                                pushToQueue ( owner, offerer, VLabel takenLabel )
                                setParticipantWithNewStack location owner 
                                    ( newMonitor
                                    , OtherSelections processSelection : otherOptionsStack 
                                    , takenProgram 
                                    ) 

                _ -> 
                    error $ "Select for owner " ++ owner ++ " needs a LocalType.Select, but got " ++ show (unFix futureType)

        (Offer owner options, _) -> 
            case unFix futureType of 
                LocalType.Offer expectedOwner selector types -> do
                    ensure (owner == expectedOwner) (error $ "Offer: owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)

                    label <- popLabelFromQueue "Offer" selector owner

                    let checkLabelHasImplemenation = 
                            let errorMessage = 
                                    "the selector has picked label " 
                                        ++ label
                                        ++ " but the offerer has no implemenation for it. Perhaps you meant one of "
                                        ++ show options
                            in 
                                Map.lookup label (Map.fromList options)
                                    |> Maybe.map pure 
                                    |> Maybe.withDefault (Except.throwError $ LabelError errorMessage) 

                        checkLabelHasType = 
                            let errorMessage = 
                                    "the selector has picked label " 
                                        ++ label
                                        ++ " but there is no type for it. Perhaps you meant one of "
                                        ++ show types
                            in
                                Map.lookup label types 
                                    |> Maybe.map pure 
                                    |> Maybe.withDefault (Except.throwError $ LabelError errorMessage) 

                    program <- checkLabelHasImplemenation 
                    tipe <- checkLabelHasType 

                    let 
                        combined = 
                            options
                                |> List.map (\(l, p) -> (\t -> (l, p, t)) <$> Map.lookup l types)
                                |> Maybe.catMaybes

                        notChosen (l, _,_) = l /= label
                        selection = Zipper.fromSegments (takeWhile notChosen combined) (label, program, tipe) (drop 1 $ dropWhile notChosen combined)

                        processSelection = Zipper.map (\(label, process, tipe) -> (label, process)) selection
                        typeSelection = Zipper.map (\(label, process, tipe) -> (label, tipe)) selection 

                        newLocalType = 
                            LocalType.createState (LocalType.backwardOffer owner selector typeSelection historyType) tipe

                        newMonitor = 
                            monitor { _localType = newLocalType }  

                    setParticipantWithNewStack location owner 
                        ( newMonitor
                        , OtherOffers processSelection : otherOptionsStack 
                        , program 
                        ) 


                _ -> 
                    error $ "Offer needs a LocalType.Offer, but got " ++ show (unFix futureType)
        
        (Application owner unevaluatedFunctionValue argument, _) -> do
            argumentName <- uniqueVariableName 

            functionValue <- evaluateValue owner unevaluatedFunctionValue

            convertToFunction <- isFunction <$> State.get
            case convertToFunction functionValue of
                Nothing -> 
                    error "function is not defined" 

                Just (variable, body) -> do
                    k <- uniqueApplicationName
                    let newLocalType = LocalType.createState (Fix $ LocalType.Application owner k historyType) futureType
                        newMonitor = 
                            monitor 
                                { _store = Map.insert argumentName argument (_store monitor)
                                , _localType = newLocalType
                                , _applicationHistory = Map.insert k (unevaluatedFunctionValue, argument) (_applicationHistory monitor)
                                }  
                                |> Session.markVariableAsUsed argumentName argumentName
                    setParticipantDefault location owner (newMonitor, renameVariable variable argumentName body)

        (Parallel p q, _) -> do
            l1 <- uniqueLocation 
            l2 <- uniqueLocation 

            let newLocations = 
                    Map.empty
                        |> Map.insert l1 p
                        |> Map.insert l2 q
                        |> Map.insert location (Fix NoOp) 

            let newMonitor = monitor { _localType = LocalType.createState ( Fix $ LocalType.Spawning location l1 l2 historyType) futureType  } 

            setParticipantDefault location owner ( newMonitor, Fix NoOp )
            setParticipantDefault l1 owner ( newMonitor, p )
            setParticipantDefault l2 owner ( newMonitor, q )

        (Let owner visibleName value continuation, _) -> do
            variableName <- uniqueVariableName 

            let newLocalType = LocalType.createState (Fix $ LocalType.Assignment owner historyType) futureType
                newMonitor = 
                    monitor 
                        { _store = Map.insert variableName (renameValue visibleName variableName value) (_store monitor)
                        , _localType = newLocalType
                        }  
                        |> Session.markVariableAsUsed visibleName variableName
            
            setParticipantDefault location owner ( newMonitor, renameVariable visibleName variableName continuation )

        (IfThenElse owner condition thenBranch elseBranch, _) -> do 
            verdict <- unsafeCastToBool <$> evaluateValue owner condition 
            
            if verdict then do
                let newLocalType = LocalType.createState (Fix $ LocalType.Branched historyType) futureType
                    newMonitor = monitor { _localType = newLocalType }  
                
                setParticipantWithNewStack location owner ( newMonitor, OtherBranch condition verdict elseBranch : otherOptionsStack, thenBranch )
            else do
                let newLocalType = LocalType.createState (Fix $ LocalType.Branched historyType) futureType
                    newMonitor = monitor { _localType = newLocalType }  
            
                setParticipantWithNewStack location owner ( newMonitor, OtherBranch condition verdict thenBranch : otherOptionsStack, elseBranch )

        ( NoOp, _ ) -> 
            return () 



checkAndPerformSend :: Location -> Participant -> Monitor Value String -> Participant -> Value -> Program Value -> Session Value (Monitor Value String, Program Value)
checkAndPerformSend location participant monitor owner payload continuation = 
    if participant == owner then do
        (historyType, localType) <- second unFix <$> validateLocalType location participant monitor
        case localType of 
            LocalType.Send expectedOwner target tipe continuationType -> do
                ensure (owner == expectedOwner) (error $ "Send owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)
                value <- evaluateValue participant payload 
                let newLocalTypeState = 
                        LocalType.createState (Fix $ LocalType.BackwardSend owner target tipe historyType) continuationType

                pushToQueue ( participant, target, value )
                return (monitor { _localType = newLocalTypeState }, continuation)

            _ -> 
                error $ "`" ++ participant ++ "`'s program wants to perform a send, but its type is " ++ show localType
    else error "invalid owner in checkAndPerformSend"


checkAndPerformReceive :: Location -> Participant -> Monitor Value String -> Participant -> Identifier -> Program Value -> Session Value (Monitor Value String, Program Value)
checkAndPerformReceive location participant monitor owner visibleName continuation = 
    if participant == owner then do
        (historyType, localType) <- second unFix <$> validateLocalType location participant monitor
        case localType of 
            LocalType.Receive expectedOwner sender tipe continuationType -> do
                ensure (owner == expectedOwner) (error $ "Receive owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)
                payload <- popFromQueue "Receive" sender owner 
                variableName <- uniqueVariableName
                let newBindings = Map.insert variableName payload $ _store monitor 
                    newLocalTypeState = 
                        LocalType.createState (Fix $ LocalType.BackwardReceive owner sender tipe historyType) continuationType

                    newMonitor = monitor 
                        { _store = newBindings
                        , _localType = newLocalTypeState 
                        }  
                        |> Session.markVariableAsUsed visibleName variableName
                    
                return (newMonitor, renameVariable visibleName variableName continuation )

            _ -> 
                error $ "`" ++ participant ++ "`'s program wants to perform a receive, but its type is " ++ show localType
    else error "invalid owner in checkAndPerformReceive"

backward :: Location -> Session Value ()
backward location = do
    -- get the program
    ExecutionState { locations, participants } <- State.get

    case Map.lookup location locations of 
        Nothing -> 
            error $ "unknown location " ++ show location 

        Just (defaultParticipant, otherOptionsStack, program) -> do
            -- get the owner/participant
            let owner = statementOwner (unFix program) |> Maybe.withDefault defaultParticipant
            case Map.lookup owner participants of 
                Nothing -> 
                    error $ "unknown participant " ++ show owner

                Just monitor -> do
                    let ( historyType, futureType ) = 
                            case _localType monitor of 
                                LocalType.Unsynchronized (x,y) -> (x,y)
                                LocalType.Synchronized (x,y) -> (x,y)

                    backwardHelper location owner monitor (historyType, futureType) program otherOptionsStack

backwardHelper location owner monitor (historyType, futureType) program otherOptionsStack =
    let 
        setParticipant_ (newMonitor, newProgram) = 
            setParticipant location owner (newMonitor, otherOptionsStack, Fix newProgram)

        setParticipantWithNewStack ( newMonitor, newStack, newProgram) = 
            setParticipant location owner ( newMonitor, newStack, Fix newProgram )
    in
    case fmap (unFix . fst) (_localType monitor) of 
        LocalType.Synchronized (LocalType.BackwardSend owner receiver tipe rest) -> do
            -- pop from the future! (not history; rolling the receive will put the action into the future)
            let newLocalType = LocalType.Unsynchronized ( rest, Fix $ LocalType.Send owner receiver tipe futureType )
            
            payload <- removeFromQueue "BackwardSend" owner receiver 

            setParticipant_
                ( monitor { _localType = newLocalType } 
                , Program.Send { owner = owner, value = payload, continuation = program } 
                )

        LocalType.Unsynchronized (LocalType.BackwardSend owner receiver tipe rest) -> do
            checkSynchronizedForTransaction owner receiver
            newMonitor <- getMonitor owner
            backward location 
    
        LocalType.Synchronized (LocalType.BackwardReceive owner sender tipe rest) -> do 
            let newLocalType = LocalType.Unsynchronized ( rest, Fix $ LocalType.Receive owner sender tipe futureType ) 

            _ <- rollQueueHistory "BackwardReceive" sender owner

            case _usedVariables monitor of 
                Binding{_visibleName, _internalName} : usedVariables -> 
                    setParticipant_
                        ( monitor { _localType = newLocalType, _store = Map.delete _internalName (_store monitor), _usedVariables = usedVariables } 
                        , Program.Receive { owner = owner, variableName = _visibleName, continuation = program } 
                        )

                [] -> 
                    error "no variable to bind"

    
        LocalType.Unsynchronized (LocalType.BackwardReceive owner sender tipe rest) -> do 
            checkSynchronizedForTransaction sender owner 
            backward location 

        LocalType.Synchronized LocalType.Selected{ owner, offerer, selection, continuation } -> do
            _ <-  removeFromQueue "BackwardSelect" owner offerer 
            let types = Zipper.toList selection 
            case otherOptionsStack of 
                OtherSelections programs : newOtherOptionsStack -> 

                    setParticipantWithNewStack
                        ( monitor 
                            { _localType = LocalType.Unsynchronized ( continuation, LocalType.select owner offerer types )                            
                            }
                        , newOtherOptionsStack 
                        , Program.Select owner (Zipper.toList programs) 
                        )
                _ -> 
                    error "backward select failed"



        LocalType.Unsynchronized LocalType.Selected{ owner, offerer } -> do
            checkSynchronizedForChoice offerer owner
            backward location 

        LocalType.Synchronized LocalType.Offered{ owner, selector, picked, continuation } -> do
            _ <- rollQueueHistory "BackwardOffer" selector owner
            let types = Zipper.toList picked 
            case otherOptionsStack of 
                OtherOffers programs : newOtherOptionsStack -> 

                    setParticipantWithNewStack
                        ( monitor 
                            { _localType = LocalType.Unsynchronized ( continuation,  LocalType.offer owner selector types )
                            }
                        , newOtherOptionsStack
                        , Program.Offer owner (Zipper.toList programs) 
                        )
                _ -> 
                    error "backward offer failed"


        LocalType.Unsynchronized LocalType.Offered{ owner, selector, picked, continuation } -> do
            checkSynchronizedForChoice owner selector
            newMonitor <- getMonitor owner
            backward location 

        LocalType.Synchronized _ -> 
            error "type is synced, but the historyType instruction is not a transaction or choice"

        LocalType.Unsynchronized (LocalType.Application owner k rest) ->
            case (Map.lookup k (_applicationHistory monitor), _usedVariables monitor) of 
                ( Just ( functionValue, argument ), Binding{_internalName}:usedVariables ) -> 
                    setParticipant_
                        ( monitor 
                            { _localType = LocalType.Unsynchronized ( rest, futureType )
                            , _store = Map.delete _internalName (_store monitor)
                            , _applicationHistory = Map.delete k (_applicationHistory monitor) 
                            , _usedVariables = usedVariables
                            } 
                        , Program.Application owner functionValue argument
                        )

                ( Nothing, _ ) -> 
                    error "rolling function that does not exist"

                ( _, [] ) -> 
                    error "rolling function application but not argument was bound"

        LocalType.Unsynchronized (LocalType.Spawning l l1 l2 rest) | l /= location -> 
            error "rolling someone else's spawn"

        LocalType.Unsynchronized (LocalType.Spawning l l1 l2 rest) -> do
            (_, p1) <- getParticipant l1 owner
            (_, p2) <- getParticipant l2 owner

            case unFix program of 
                NoOp -> do
                    let newLocalType = LocalType.Unsynchronized ( rest, futureType )
                    removeLocation l1
                    removeLocation l2

                    setParticipant_( monitor { _localType = newLocalType }, Program.Parallel p1 p2 )

                _ -> 
                    error "rolling a program that is not NoOp"

        LocalType.Unsynchronized (LocalType.Assignment owner rest) -> 
            case _usedVariables monitor of 
                Binding{_visibleName, _internalName} : usedVariables -> 
                    case Map.lookup _internalName (_store monitor) of 
                        Just value -> 
                            let 
                                newMonitor = 
                                    monitor 
                                        { _store = Map.delete _internalName (_store monitor)
                                        , _localType = LocalType.Unsynchronized (rest, futureType)
                                        , _usedVariables = usedVariables 
                                        }  
                            in            
                                setParticipant_
                                    ( newMonitor
                                    , Program.Let owner _visibleName value $ renameVariable _internalName _visibleName program
                                    )

                        Nothing -> 
                            error "undoing assignment to variable that does not exist"

                [] -> 
                    error "rolling an assignment, but no variables are used"

        LocalType.Unsynchronized (LocalType.Branched rest) -> 
            let 
                newMonitor = 
                    monitor { _localType = LocalType.Unsynchronized (rest, futureType) }  
            in 
                case otherOptionsStack of 
                    OtherBranch condition verdict otherBranch : rest -> 
                        setParticipantWithNewStack
                            ( newMonitor
                            , rest
                            , if verdict then Program.IfThenElse owner condition program otherBranch else Program.IfThenElse owner condition otherBranch program
                            )
                    _ -> 
                        error "no other branch"

        LocalType.Unsynchronized (LocalType.BackwardRecursionPoint rest) -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = LocalType.Unsynchronized (rest, LocalType.recurse futureType)
                        , _recursionPoints = drop 1 (_recursionPoints monitor)
                        }  
            in
                setParticipant_
                    ( newMonitor
                    , unFix program 
                    )

        LocalType.Unsynchronized (LocalType.BackwardWeakenRecursion rest) -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = LocalType.Unsynchronized (rest, LocalType.broadenScope futureType) 
                        , _recursiveVariableNumber = _recursiveVariableNumber monitor - 1
                        }  
            in
                setParticipant_
                    ( newMonitor
                    , unFix program 
                    )

        LocalType.Unsynchronized (LocalType.BackwardRecursionVariable rest) -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = LocalType.Unsynchronized (rest, LocalType.recursionVariable)
                        }  
            in
                setParticipant_
                    ( newMonitor
                    , unFix program 
                    )

        LocalType.Unsynchronized LocalType.Hole -> 
            return () 

        LocalType.Unsynchronized (LocalType.LocalType _ _) -> 
            error $ "satisfy the exhaustiveness checker" ++ show (unFix historyType)



validate :: Location -> Participant -> Session Value (Program Value, Monitor Value String) 
validate location participant = do
    state <- State.get
    case Map.lookup participant (participants state) of 
        Nothing -> 
            error "unknown participant"
        Just monitor -> 
            case Map.lookup location (locations state) of 
                Just (_, _, program) -> 
                    return (program, monitor)

                Nothing -> 
                    error $ "location has no participant named " ++ participant



type Id = (Location, Participant)
type Type = String

wrapLocalType :: (LocalType u -> LocalType u) -> Monitor Value u -> Monitor Value u
wrapLocalType tagger monitor = 
    let 
        newLocalType = LocalType.mapState (\history future -> (history, tagger future)) (_localType monitor)
    in
        monitor { _localType =  newLocalType }

{-
forwardChoice :: Id -> Id -> Map String (GlobalType.GlobalType Participant String) -> Session Value String
forwardChoice (l1, offerer) (l2, selector) options = do
    -- validate both parties
    (program1, monitor1_) <- validate l1 offerer
    (program2, monitor2_) <- validate l2 selector

    let participants = Set.empty -- <- obviously wrong 

    -- augment their types
    let monitor1 = wrapLocalType (\_ -> Fix $ LocalType.Offer  offerer selector (fmap (LocalType.project participants offerer)  options)) monitor1_
        monitor2 = wrapLocalType (\_ -> Fix $ LocalType.Select offerer selector (fmap (LocalType.project participants selector) options)) monitor2_

    -- move actors forward
    forward l2 selector program2 monitor2
    forward l1 offerer  program1 monitor1
    
    -- inspect local type context to know which label was picked
    localTypeHistory <- fst . LocalType.unwrapState . _localType . snd <$> validate l2 selector
    case unFix localTypeHistory of 
        LocalType.Selected _ _ selection _ -> do 
            let (label, _, _, _) = Zipper.current selection
            return label 

        _ -> 
            error "could not unpack label"

forwardTransaction :: Id -> Id -> Type -> Session Value ()
forwardTransaction (l1, sender) (l2, receiver) tipe = do 
    -- validate both parties
    (program1, monitor1_) <- validate l1 sender
    (program2, monitor2_) <- validate l2 receiver

    -- augment their types
    let monitor1 = wrapLocalType (Fix . LocalType.Send    sender receiver tipe) monitor1_
        monitor2 = wrapLocalType (Fix . LocalType.Receive receiver sender tipe) monitor2_

    forward l1 sender   program1 monitor1
    forward l2 receiver program2 monitor2

untilTypeDecision :: (Location -> Participant -> Program Value -> Monitor Value String -> Session Value ()) 
                  -> Location -> Participant -> Session Value ()
untilTypeDecision mover location participant = do
    (program, monitor) <- validate location participant 
    case unFix program of 
        Send {} -> return ()
        Receive {} -> return ()
        Offer _ _ -> return ()
        Select _ _ ->return ()
        _ -> do
            mover location participant program monitor
            untilTypeDecision mover location participant  

forwardUntilTypeDecision :: Location -> Participant -> Session Value ()
forwardUntilTypeDecision = untilTypeDecision forward 

backwardUntilTypeDecision :: Location -> Participant -> Session Value ()
backwardUntilTypeDecision = untilTypeDecision backward 

-}

forward_ :: Location -> Session Value ()        
forward_ = forward 



forwardTestable :: Location -> ExecutionState Value -> Either Error (ExecutionState Value)
forwardTestable location state = 
    fmap snd $ Except.runExcept $ State.runStateT (forward location) state

backwardTestable :: Location -> ExecutionState Value -> Either Error (ExecutionState Value)
backwardTestable location state = 
    fmap snd $ Except.runExcept $ State.runStateT (backward location) state





