{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
module MicroOz 
    (Program(..)
    , History(..)
    , Value(..)
    , Execution
    , StackAction(..)
    , ForwardMsg(..)
    , BackwardMsg(..)
    , Consumed(..)
    , advanceP
    , backwardP
    , MicroOz.init
    , IntOperator(..)
    , BooleanOperator(..)
    ) where


import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Applicative (liftA2)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import qualified Utils.Maybe as Maybe
import qualified Utils.Result as Result

import qualified SessionType
import SessionType (LocalAtom(sender, receiver, type_), LocalTypeState(participant))
import qualified Queue 
import Data.Thread as Thread
import Data.ThreadState (ThreadState) 
import Data.Context as Context 
import Data.PID as PID (PID, create, parent, child)
import Data.Identifier as Identifier (Identifier, ChannelName)
import Data.Expr
import Data.Actor as Actor (Participant, Actor, named, unnamed, push, pop, toList)

import Types
import Cmd (Cmd)
import qualified Cmd 
import Debug.Trace as Debug

import GHC.Generics
import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Monoid ((<>))

{-| The main type keeping the context of the execution

It's a `StateT` that has read and write access to a tuple of two values

* `Context Value` stores variable bindings, channels and handles fresh variable name generation
* `ThreadState History Program` contains the threads 

Additionally, any computation `Either` succeed, or fail producing an `Error`.

Finally, a value of type `a` is produced.
-}
type Execution a = StateT (Context Value) (Either Error) a

type Queue = Queue.Queue
type QueueHistory = Queue.QueueHistory


init :: SessionType.GlobalType -> Map.Map Participant (SessionType.LocalType String) -> Program -> ( Context Value, Thread History Program ) 
init globalType types program = 
    let 
        thread = Thread (PID.create [ 0 ]) Actor.unnamed [] [ program ] 
    in 
        ( Context.singleton globalType types thread, thread ) 

{-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
data Value 
    = Receive { channelName :: Identifier } 
    | Procedure (List Identifier) Program
    | Port 
    | VInt IntExpr 
    | VBool BoolExpr
    | VUnit 
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)


{-| Data type representing actions that have lead to the current program state -} 
data History 
    = Skipped
    | Composed 
    | Sent { channelName :: Identifier, payload :: Identifier }
    | Received { channelName :: Identifier, binding :: Identifier, variableName :: Identifier }
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread Actor PID
    | BranchedOn BoolExpr Bool Program 
    | AssertedOn BoolExpr 
    | ChangedActor (StackAction Participant)
    deriving (Eq, Show, Generic, ElmType, ToJSON, FromJSON)

data StackAction a = Pop a | Push a
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

{-| The µOz Syntax -}            
data Program 
    = Sequence Program Program
    | Let Identifier Value Program
    | If BoolExpr Program Program
    | SpawnThread Actor Program
    | Skip
    | Apply Identifier (List Identifier)
    | Send { channelName :: Identifier, payload :: Identifier } 
    | Assert BoolExpr
    | ChangeActor (StackAction Participant) 
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

instance Monoid Program where 
    mappend = Sequence
    mempty = Skip

-- EVALUATION

evalIntExpr :: (MonadState (Context Value) m, MonadError Error m) => IntExpr -> m Int
evalIntExpr expression =
    case expression of
        LiteralInt value -> 
            return value 

        IntOperator op left right ->
            liftA2 (intOperatorToFunction op) (evalIntExpr left) (evalIntExpr right) 

        ReferenceInt identifier -> do
            value <- lookupVariable identifier 
            case value of
                VInt value -> 
                    evalIntExpr value

                other -> 
                    Except.throwError $ TypeError identifier "I expected a integer value but got" (show other)


evalBoolExpr :: (MonadState (Context Value) m, MonadError Error m) => BoolExpr -> m Bool
evalBoolExpr expression =
    case expression of
        LiteralBool value -> 
            return value 

        BoolOperator op left right ->
            liftA2 (boolOperatorToFunction op) (evalIntExpr left) (evalIntExpr right) 

        ReferenceBool identifier -> do
            value <- lookupVariable identifier 
            case value of
                VBool value -> 
                    evalBoolExpr value

                other -> 
                    Except.throwError $ TypeError identifier "I expected a boolean value but got" (show other)


-- RENAMING

{-| Rename a variable in the whole expression

Renaming is used in evaluating a function, where the paramater names in the body 
are renamed to globally available variables.
-} 
renameVariable :: Identifier -> Identifier -> Program -> Program
renameVariable old new program = 
    let tagger id = if id == old then new else id in
    case program of
        Sequence a b -> 
            Sequence (renameVariable old new a) (renameVariable old new b)

        Let name value continuation -> 
            if name == old then
                -- name clashing, don't do anything
                Let name value continuation

            else 
                Let name (renameVariableInValue old new value) (renameVariable old new continuation)

        If condition trueBody falseBody -> 
            If (renameBoolExpr old new condition) (renameVariable old new trueBody) (renameVariable old new falseBody)

        SpawnThread name work ->
            SpawnThread name (renameVariable old new work)

        Skip -> Skip

        Assert condition -> Assert (renameBoolExpr old new condition)

        Apply f args -> 
            Apply (tagger f) (map tagger args)

        Send { channelName, payload } -> 
            Send (tagger channelName) (tagger payload) 

        ChangeActor actor ->
            ChangeActor actor 

renameVariableInValue :: Identifier -> Identifier -> Value -> Value 
renameVariableInValue old new value = 
    case value of
        VBool value -> 
            VBool (renameBoolExpr old new value)

        VUnit ->
            VUnit

        Receive { channelName } -> 
            Receive (if channelName == old then new else channelName) 

        Procedure arguments body -> 
            if old `elem` arguments then
                value
            else
                Procedure arguments $ renameVariable old new body

        Port -> Port

        VInt value -> 
            VInt (renameIntExpr old new value)


-- HELPERS

asParticipant :: Participant -> Program -> Program
asParticipant participant program = 
    foldl Sequence Skip [ ChangeActor (Push participant), program, ChangeActor (Pop participant) ]



-- ATOMIC STEPS for a single thread

{- messages are signals to the "global" state to influence other threads based on this thread's actions -} 

data BackwardMsg 
    = RollChild { caller :: PID, toRoll :: PID, actor :: Actor } 
    | RollSend { caller :: PID, histories :: List (QueueHistory String), channelName :: ChannelName } 
    | RollReceive { caller :: PID, histories :: List (QueueHistory String), channelName :: ChannelName } 
    deriving (Show)


data ForwardMsg
    = Spawn (Thread History Program) 
    | PushOnStack Program
    deriving (Show)


receive :: 
        PID 
        -> Actor
        -> (Identifier, Value, Program) 
        -> ChannelName 
        -> LocalTypeState String 
        -> Execution (LocalTypeState String, (History, Actor, Cmd.Cmd ForwardMsg))
receive pid currentActor (identifier, value, continuation) channelName localTypeState = 
    SessionType.forwardWithReceive localTypeState
        |> Result.mapError formatError
        |> Result.map handleMessage
        |> Result.unwrap

  where 
    formatError e = 
        error $ "The session type won't allow moving forward with a send" ++ show e

    handleMessage :: (Participant, String, LocalTypeState String)
              -> Execution (LocalTypeState String, (History, Actor, Cmd ForwardMsg)) 
    handleMessage  ( expectedSender, valueType, newLocalTypeState ) = do
        participant <- currentParticipant currentActor 
        message <- readChannel expectedSender participant valueType channelName 

        let assign channelName value = do
                variableName <- Context.insertVariable pid value 
                return 
                    ( Received{ channelName = channelName, binding = identifier, variableName = variableName } 
                        , currentActor 
                        , Cmd.create (PushOnStack $ renameVariable identifier variableName continuation)  
                    ) 
        message
            |> Maybe.map (\value -> (,) newLocalTypeState <$> assign channelName value)
            |> Maybe.withDefault (Except.throwError $ BlockedOnReceive pid)


{-| Take one step forward -} 
advanceP :: PID -> Actor -> Program -> Execution ( History, Actor, Cmd ForwardMsg)
advanceP pid currentActor program = 
    case program of
        Skip ->
            return
                ( Skipped
                , currentActor
                , Cmd.none
                )

        Sequence a b ->
            return 
                ( Composed
                , currentActor
                , Cmd.batch [ Cmd.create (PushOnStack b), Cmd.create (PushOnStack a) ]
                )

        ChangeActor action -> 
            let 
                newActor = 
                    case action of 
                        Pop participant -> Actor.pop currentActor
                        Push participant -> Actor.push participant currentActor
            in
                return ( ChangedActor action, newActor, Cmd.none ) 

        Let identifier value continuation -> 
            case value of
                Receive { channelName } ->
                    receive pid currentActor (identifier, value, continuation) channelName
                        |> Context.modifyLocalTypeStateM currentActor 

                Port -> do
                    channelName <- Context.insertChannel pid Queue.empty
                    return 
                        ( CreatedChannel channelName
                        , currentActor
                        , Cmd.create (PushOnStack $ renameVariable identifier channelName continuation)
                        )

                Procedure arguments body -> do
                    functionName <- Context.insertBinding pid $ \functionName -> 
                        -- rename the function name itself in the body, for recursive functions
                        let renamedBody = 
                                if identifier `notElem` arguments then
                                    renameVariable identifier functionName body
                                else
                                    body

                        in
                            Procedure arguments renamedBody 

                    return 
                        ( CreatedVariable functionName                            
                        , currentActor
                        , Cmd.create (PushOnStack $ renameVariable identifier functionName continuation)
                        )

                    

                _ -> do
                    variableName <- Context.insertVariable pid value 
                    return 
                        ( CreatedVariable variableName
                        , currentActor
                        , Cmd.create (PushOnStack $ renameVariable identifier variableName continuation)
                        )


        If condition trueBody falseBody -> do
            verdict <- evalBoolExpr condition 
            let ( history, body ) = 
                    if verdict then
                        ( BranchedOn condition True falseBody, trueBody )
                    else
                        (BranchedOn condition False trueBody, falseBody )
            return ( history, currentActor, Cmd.create (PushOnStack body) )

        Assert condition -> do
            verdict <- evalBoolExpr condition 
            if not verdict then
               Except.throwError $ AssertionError (show condition)
            else
                return ( AssertedOn condition, currentActor, Cmd.none ) 


        SpawnThread actor work -> do
            thread@(Thread threadName _ _ _) <- Context.insertThread pid actor [] [ work ] 

            return 
                ( SpawnedThread actor threadName
                , currentActor
                , Cmd.create (Spawn thread)
                ) 

        Apply functionName arguments -> do
            ( parameters, body ) <- lookupProcedure functionName 
            if length arguments /= length parameters then
                Except.throwError $ ArgumentMismatch functionName (length parameters) (length arguments) 
            else 
                let
                    withRenamedVariables = 
                            foldr (uncurry renameVariable) body (zip parameters arguments)
                in
                    return 
                        ( CalledProcedure functionName arguments
                        , currentActor
                        , Cmd.create (PushOnStack withRenamedVariables)
                        ) 

                        
        Send { channelName, payload } -> 
            Context.modifyLocalTypeStateM currentActor $ \localTypeState -> 
                case SessionType.forwardWithSend localTypeState of 
                    Right (expectedReceiver, valueType, newLocalTypeState) -> do
                        participant <- currentParticipant currentActor 
                        value <- procedureAsParticipant participant <$> lookupVariable payload 
                        writeChannel participant expectedReceiver valueType channelName value 
                        return 
                            ( newLocalTypeState
                                , ( Sent channelName payload 
                                , currentActor 
                                , Cmd.none 
                                  )
                            )

                    Left e -> 
                        -- Except.throwError $ BlockedOnReceive pid
                        error "wants to send but session type won't allow it"




{-| Wrap a function into a block that 
changes the participant. Functions sent over a channel 
are evaluated as the participant that sent them.
-}
procedureAsParticipant :: Participant -> Value -> Value 
procedureAsParticipant participant value = 
    case value of 
        Procedure arguments body -> 
            Procedure arguments (asParticipant participant body)

        _ -> 
            value



tracer f a = Debug.trace (f a) a

{-| sometimes a history is not actually handled
For instance because something else must be rolled first.
This value communicates this fact to the caller 
-}
data Consumed = Consumed | Unconsumed


{-| Take one step backward -} 
backwardP :: (MonadState (Context Value) m, MonadError Error m) 
          => PID 
          -> Actor
          -> History  
          -> List Program 
          -> m ( Consumed, Actor, List Program, Cmd.Cmd BackwardMsg ) 
backwardP pid currentActor history program = do
    context <- State.get
    let continue newProgram =   return ( Consumed, currentActor, newProgram, Cmd.none )
        f (h, p) =  unwords [ show pid , show h, show p ] ++ "\n\n"
    case tracer f (history, program) of 
        ( Skipped, _ ) ->
            continue (Skip : program)

        ( Composed, first:second:restOfProgram ) ->
            continue (Sequence first second : restOfProgram)

        ( ChangedActor action, restOfProgram ) -> 
            let
                oldActor = 
                    case action of 
                        Pop participant -> Actor.push participant currentActor
                        Push participant -> Actor.pop currentActor
            in
            return ( Consumed, oldActor, ChangeActor action : restOfProgram, Cmd.none )

        ( CreatedVariable identifier, continuation : restOfProgram ) -> do
            value <- Context.lookupVariable identifier 
            State.modify $ removeVariable identifier 
            continue (Let identifier value continuation : restOfProgram )

        ( CreatedChannel identifier, continuation : restOfProgram ) -> do
            Context.removeChannel identifier
            continue (Let identifier Port continuation : restOfProgram)

        ( BranchedOn condition True falseBody, trueBody : restOfProgram ) ->
            continue (If condition trueBody falseBody : restOfProgram)
            
        ( BranchedOn condition False trueBody, falseBody : restOfProgram ) ->
            continue (If condition trueBody falseBody : restOfProgram)

        ( AssertedOn condition, restOfProgram ) ->
            continue (Assert condition : restOfProgram)

        ( CalledProcedure functionName arguments, body : restOfProgram ) ->
            continue (Apply functionName arguments : restOfProgram)

        ( Sent { channelName, payload }, restOfProgram ) -> 
            Context.modifyLocalTypeStateM currentActor $ \localTypeState -> 
                case SessionType.backwardWithSend localTypeState of
                    Left e -> 
                        error $ show e

                    Right ( receiver, newLocalTypeState, valueType ) -> 
                        withChannel channelName $ \queue -> do
                            participant <- currentParticipant currentActor 
                            case Queue.rollPush participant receiver valueType queue of
                                Right (newChannel, _) -> do
                                    mapChannel channelName (const newChannel)

                                    return  ( newLocalTypeState,
                                        ( Consumed
                                        , currentActor 
                                        , Send channelName payload  : restOfProgram
                                        , Cmd.none 
                                        ))
                                Left (Queue.InvalidAction _ _) -> do
                                    -- generate the list of actions on the channel that need to be undone
                                    -- including the current one: processing of the message will make us go
                                    -- into the `then` branch above.
                                    toRollFirst <- withChannel channelName (return . Queue.followingReceive participant)
                                    return 
                                        ( localTypeState
                                            , ( Unconsumed
                                        , currentActor
                                        , restOfProgram
                                        , Cmd.create RollSend{ caller = pid, histories =  toRollFirst, channelName = channelName }
                                        ))

                                Left e -> error $ "backward evaluation should not fail, but got" ++ show e


        ( Received{ channelName, binding, variableName }, continuation : restOfProgram ) -> do
            value <- Context.lookupVariable variableName 
            State.modify $ removeVariable variableName 
            Context.modifyLocalTypeStateM currentActor $ \localTypeState ->
                case SessionType.backwardWithReceive localTypeState of
                    Left e -> 
                        error $ show e

                    Right ( sender, newLocalTypeState, valueType ) ->
                        withChannel channelName $ \queue -> do
                            participant <- currentParticipant currentActor 

                            case Queue.rollPop participant sender valueType value queue of
                                Right newChannel -> do
                                    -- set the channel to the new version
                                    mapChannel channelName (const newChannel)

                                    return 
                                        ( newLocalTypeState
                                            , ( Consumed
                                        , currentActor
                                        , Let binding (Receive channelName) (renameVariable variableName binding continuation) : restOfProgram 
                                        , Cmd.none 
                                        ))

                                Left (Queue.InvalidAction _ _) -> do
                                    -- generate the list of actions on the channel that need to be undone
                                    -- including the current one: processing of the message will make us go
                                    -- into the `then` branch above.
                                    toRollFirst <- withChannel channelName (return . Queue.followingReceive participant)

                                    return 
                                        ( localTypeState 
                                            , ( Unconsumed
                                            , currentActor 
                                        , restOfProgram
                                        , Cmd.create RollReceive{ caller = pid, histories =  toRollFirst, channelName = channelName }
                                        ))

                                Left e -> 
                                    error $ "backward evaluation should not fail, but got" ++ show e

        ( SpawnedThread participant toRoll, restOfProgram ) -> 
            return 
                ( Consumed 
                , currentActor
                , restOfProgram
                , Cmd.create (RollChild pid toRoll participant)
                )

        ( _, restOfProgram) ->
            -- the program has a pattern incompatible with the history instruction we're currently matching
            error $ show history ++ " encountered a pattern it cannot match: " ++ show restOfProgram 


-- HELPERS


lookupProcedure :: (MonadState (Context Value) m, MonadError Error m) => Identifier -> m (List Identifier, Program)
lookupProcedure identifier = do
    value <- lookupVariable identifier 
    case value of
        Procedure arguments body -> 
            return (arguments, body)

        _ ->
            Except.throwError $ TypeError identifier "I expected a Procedure but instead got" (show value)
