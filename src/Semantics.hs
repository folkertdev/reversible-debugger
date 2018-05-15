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


validateLocalType :: Location -> Participant -> Monitor Value String -> Session Value (LocalType.TypeContext (Program Value) Value String, LocalType String)
validateLocalType location participant monitor = 
    case _localType monitor of 
        LocalType.Unsynchronized x -> 
            return x 

        LocalType.Synchronized _ -> 
            Except.throwError $ SynchronizationError $ "cannot move forward a synchronized local type: it has to take a step back first. For participant " ++ participant ++ "at location " ++ location


 
forward :: Location -> Participant -> Program Value -> Monitor Value String -> Session Value ()  
forward location participant program monitor = do
    (previous, fixedLocal) <- validateLocalType location participant monitor
    let localType = unFix fixedLocal
    case ( unFix program, localType) of 
        ( _, LocalType.RecursionPoint rest ) ->
            let 
                newLocalType = LocalType.createState (Fix $ LocalType.BackwardRecursionPoint previous) rest
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
                newLocalType = LocalType.createState (Fix $ LocalType.BackwardWeakenRecursion previous) rest 
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
                index = _recursiveVariableNumber monitor
                options = _recursionPoints monitor
                continuationType = 
                    case drop index options of 
                        [] -> error $ "recursion to nothing, looking for index " ++ show index ++ " in " ++ show options
                        x:xs -> 
                            x

                newLocalType = LocalType.createState (Fix $ LocalType.BackwardRecursionVariable previous) continuationType
                newMonitor = 
                    monitor 
                        { _localType = newLocalType
                        -- , _recursionPoints = take (_recursiveVariableNumber monitor + 1) (_recursionPoints monitor) 
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
                                newLocalType = LocalType.createState (LocalType.backwardSelect owner offerer selection previous) takenType
                                newMonitor = 
                                    monitor { _localType = newLocalType }  
                            in do
                                pushToQueue ( offerer, participant, VLabel takenLabel )
                                setParticipant location participant ( newMonitor, takenProgram ) 

                _ -> 
                    error $ "Select for owner " ++ owner ++ " needs a LocalType.Select, but got " ++ show localType

        (Offer owner options, _) -> 
            case localType of 
                LocalType.Offer expectedOwner selector types -> do
                    ensure (owner == expectedOwner) (error $ "Offer: owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)

                    label <- popLabelFromQueue "Offer" owner selector



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

                        newLocalType = 
                            LocalType.createState (LocalType.backwardOffer owner selector selection previous) tipe

                        newMonitor = 
                            monitor { _localType = newLocalType }  

                    setParticipant location participant ( newMonitor, program ) 

                _ -> 
                    error $ "Offer needs a LocalType.Offer, but got " ++ show localType
        
        (Application functionName argument, _) -> do
            functionValue <- lookupVariable participant functionName 
            argumentName <- uniqueVariableName 

            convertToFunction <- isFunction <$> State.get
            case convertToFunction functionValue of
                Nothing -> 
                    error "function is not defined" 

                Just (variable, body) -> do
                    k <- uniqueApplicationName
                    let newLocalType = LocalType.createState (Fix $ LocalType.Application argumentName k previous) fixedLocal
                        newMonitor = 
                            monitor 
                                { _store = Map.insert argumentName argument (_store monitor)
                                , _localType = newLocalType
                                , _applicationHistory = Map.insert k (functionName, argument) (_applicationHistory monitor)
                                }  
                    setParticipant location participant (newMonitor, renameVariable variable argumentName body)

        (Parallel p q, _) -> do
            l1 <- uniqueLocation 
            l2 <- uniqueLocation 

            let newLocations = 
                    Map.empty
                        |> Map.insert l1 p
                        |> Map.insert l2 q
                        |> Map.insert location (Fix NoOp) 

            let newMonitor = monitor { _localType = LocalType.createState ( Fix $ LocalType.Spawning location l1 l2 previous) fixedLocal  } 

            setParticipant location participant ( newMonitor, Fix NoOp )
            setParticipant l1 participant ( newMonitor, p )
            setParticipant l2 participant ( newMonitor, q )

        (Let visibleName value continuation, _) -> do
            variableName <- uniqueVariableName 

            let newLocalType = LocalType.createState (Fix $ LocalType.Assignment visibleName variableName previous) fixedLocal
                newMonitor = 
                    monitor 
                        { _store = Map.insert variableName (renameValue visibleName variableName value) (_store monitor)
                        , _localType = newLocalType
                        }  
            
            setParticipant location participant ( newMonitor, renameVariable visibleName variableName continuation )

        (IfThenElse condition thenBranch elseBranch, _) -> do 
            verdict <- unsafeCastToBool <$> evaluateValue participant condition 
            
            if verdict then do
                let newLocalType = LocalType.createState (Fix $ LocalType.Branched condition verdict elseBranch previous) fixedLocal
                    newMonitor = monitor { _localType = newLocalType }  
                
                setParticipant location participant ( newMonitor, thenBranch )
            else do
                let newLocalType = LocalType.createState (Fix $ LocalType.Branched condition verdict thenBranch previous) fixedLocal
                    newMonitor = monitor { _localType = newLocalType }  
            
                setParticipant location participant ( newMonitor, elseBranch )


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
    if participant == owner then do
        (previous, localType) <- second unFix <$> validateLocalType location participant monitor
        case localType of 
            LocalType.Send expectedOwner target tipe continuationType -> do
                ensure (owner == expectedOwner) (error $ "Send owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)
                value <- evaluateValue participant payload 
                let newLocalTypeState = 
                        LocalType.createState (Fix $ LocalType.BackwardSend owner target tipe previous) continuationType

                pushToQueue ( participant, target, value )
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
    if participant == owner then do
        (previous, localType) <- second unFix <$> validateLocalType location participant monitor
        case localType of 
            LocalType.Receive expectedOwner sender tipe continuationType -> do
                ensure (owner == expectedOwner) (error $ "Receive owners don't match: got " ++ owner ++ " but the type expects " ++ expectedOwner)
                payload <- popFromQueue "Receive" sender owner 
                variableName <- uniqueVariableName
                let newBindings = Map.insert variableName payload $ _store monitor 
                    newLocalTypeState = 
                        LocalType.createState (Fix $ LocalType.BackwardReceive owner sender visibleName variableName tipe previous) continuationType

                    newMonitor = monitor { _store = newBindings, _localType = newLocalTypeState }  
                    
                setParticipant location participant (newMonitor, renameVariable visibleName variableName continuation )

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


backward :: Location -> Participant -> Program Value -> Monitor Value String -> Session Value ()  
backward location participant program monitor = 
    let (previous, next) = LocalType.unwrapState $ _localType monitor 
        setParticipant_ (m, progF) = setParticipant location participant (m, Fix progF)
    in
    case fmap (unFix . fst) (_localType monitor) of 
        LocalType.Synchronized (LocalType.BackwardSend owner receiver tipe rest) -> do
            -- pop from the future! (not history; rolling the receive will put the action into the future)
            let newLocalType = LocalType.Unsynchronized ( rest, Fix $ LocalType.Send owner receiver tipe next )
            
            payload <- removeFromQueue "BackwardSend" owner receiver 

            setParticipant_
                ( monitor { _localType = newLocalType } 
                , Program.Send { owner = owner, value = payload, continuation = program } 
                )

        LocalType.Unsynchronized (LocalType.BackwardSend owner receiver tipe rest) -> do
            checkSynchronizedForTransaction owner receiver
            newMonitor <- getMonitor owner
            backward location owner program newMonitor
    
        LocalType.Synchronized (LocalType.BackwardReceive owner sender visibleName variableName tipe rest) -> do 
            let newLocalType = LocalType.Unsynchronized ( rest, Fix $ LocalType.Receive owner sender tipe next ) 

            _ <- rollQueueHistory "BackwardReceive" sender owner

            setParticipant_
                ( monitor { _localType = newLocalType, _store = Map.delete variableName (_store monitor) } 
                , Program.Receive { owner = owner, variableName = visibleName, continuation = program } 
                )

    
        LocalType.Unsynchronized (LocalType.BackwardReceive owner sender visibleName variableName tipe rest) -> do 
            checkSynchronizedForTransaction sender owner 
            newMonitor <- getMonitor owner
            backward location owner program newMonitor

        LocalType.Synchronized LocalType.Selected{ owner, offerer, selection, continuation } -> do
            _ <-  removeFromQueue "BackwardSelect" offerer owner 
            let 
                combined = Zipper.toList selection 
                types = List.map (\(l, _,_,t) -> (l, t)) combined 
                options = List.map (\(label, condition, program, _) -> (label, condition, program)) combined

            setParticipant_
                ( monitor { _localType = LocalType.Unsynchronized ( continuation,  LocalType.select owner offerer types )}
                , Program.Select owner options 
                )


        LocalType.Unsynchronized LocalType.Selected{ owner, offerer } -> do
            checkSynchronizedForChoice offerer owner
            newMonitor <- getMonitor owner
            backward location owner program newMonitor

        LocalType.Synchronized LocalType.Offered{ owner, selector, picked, continuation } -> do
            _ <- rollQueueHistory "BackwardOffer" owner selector
            let 
                combined = Zipper.toList picked 
                (programs, types) =
                   combined
                        |> List.map (\(label, program, tipe) -> ((label, program), (label, tipe)))
                        |> List.unzip

            setParticipant_
                ( monitor { _localType = LocalType.Unsynchronized ( continuation,  LocalType.offer owner selector types )}
                , Program.Offer owner programs 
                )


        LocalType.Unsynchronized LocalType.Offered{ owner, selector, picked, continuation } -> do
            checkSynchronizedForChoice owner selector
            newMonitor <- getMonitor owner
            backward location owner program newMonitor

        LocalType.Synchronized _ -> 
            error "type is synced, but the previous instruction is not a transaction or choice"

        LocalType.Unsynchronized (LocalType.Application argumentName k rest) ->
            case Map.lookup k (_applicationHistory monitor) of 
                Nothing -> 
                    error "rolling function that does not exist"
                Just ( functionName, argument ) -> 
                    setParticipant_
                        ( monitor 
                            { _localType = LocalType.Unsynchronized ( rest, next )
                            , _store = Map.delete argumentName (_store monitor)
                            , _applicationHistory = Map.delete k (_applicationHistory monitor) 
                            } 
                        , Program.Application functionName argument
                        )


        LocalType.Unsynchronized (LocalType.Spawning l l1 l2 rest) | l /= location -> 
            error "rolling someone else's spawn"

        LocalType.Unsynchronized (LocalType.Spawning l l1 l2 rest) -> do
            (_, p1) <- getParticipant l1 participant
            (_, p2) <- getParticipant l2 participant

            case unFix program of 
                NoOp -> do
                    let newLocalType = LocalType.Unsynchronized ( rest, next )
                    removeLocation l1
                    removeLocation l2

                    setParticipant_( monitor { _localType = newLocalType }, Program.Parallel p1 p2 )

                _ -> 
                    error "rolling a program that is not NoOp"

        LocalType.Unsynchronized (LocalType.Assignment visibleName variableName rest) -> 
            let 
                store = _store monitor 

                value = 
                    Map.lookup variableName store
                        |> fromMaybe (error "undoing assignment to variable that does not exist")

                newMonitor = 
                    monitor 
                        { _store = Map.delete variableName store
                        , _localType = LocalType.Unsynchronized (rest, next)
                        }  
            in            
                setParticipant_
                    ( newMonitor
                    , Program.Let visibleName value $ renameVariable variableName visibleName program
                    )

        LocalType.Unsynchronized (LocalType.Branched condition verdict otherBranch rest) -> 
            let 
                newMonitor = 
                    monitor { _localType = LocalType.Unsynchronized (rest, next) }  
            in
                setParticipant_
                    ( newMonitor
                    , if verdict then Program.IfThenElse condition program otherBranch else Program.IfThenElse condition otherBranch program
                    )


        LocalType.Unsynchronized (LocalType.Literal variableName rest) -> 
            let 
                store = _store monitor 

                value = 
                    Map.lookup variableName store
                        |> fromMaybe (error "undoing literal that does not exist")

                newMonitor = 
                    monitor 
                        { _store = Map.delete variableName store
                        , _localType = LocalType.Unsynchronized (rest, next)
                        }  
            in            
                setParticipant_
                    ( newMonitor
                    , Program.Literal value 
                    )

        LocalType.Unsynchronized (LocalType.BackwardRecursionPoint rest) -> 
            let
                newMonitor = 
                    monitor 
                        { _localType = LocalType.Unsynchronized (rest, LocalType.recurse next)
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
                        { _localType = LocalType.Unsynchronized (rest, LocalType.broadenScope next) 
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
            error $ "satisfy the exhaustiveness checker" ++ show (unFix previous)



validate :: Location -> Participant -> Session Value (Program Value, Monitor Value String) 
validate location participant = do
    state <- State.get
    case Map.lookup participant (participants state) of 
        Nothing -> 
            error "unknown participant"
        Just monitor -> 
            case Map.lookup location (locations state) >>= Map.lookup participant of 
                Just program -> 
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

forward_ :: Location -> Participant -> Session Value ()        
forward_ location participant = do
    (program, monitor) <- validate location participant 
    forward location participant program monitor


backward_ :: Location -> Participant -> Session Value ()        
backward_ location participant = do
    (program, monitor) <- validate location participant 
    backward location participant program monitor

forwardTestable :: Location -> Participant -> ExecutionState Value -> Either Error (ExecutionState Value)
forwardTestable location participant state = 
    fmap snd $ Except.runExcept $ State.runStateT (forward_ location participant) state

backwardTestable :: Location -> Participant -> ExecutionState Value -> Either Error (ExecutionState Value)
backwardTestable location participant state = 
    fmap snd $ Except.runExcept $ State.runStateT (backward_ location participant) state





