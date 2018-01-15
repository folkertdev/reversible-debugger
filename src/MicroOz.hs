{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns, GADTs, StandaloneDeriving, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, DefaultSignatures, OverloadedStrings #-}
module MicroOz 
    (Program(..)
    , History(..)
    , Value(..)
    , ForwardMsg(..)
    , BackwardMsg(..)
    , advanceP
    , backwardP
    , MicroOz.init
    , IntOperator(..)
    , BooleanOperator(..)
    , renameCreator
    ) where


import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Applicative (liftA2)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import qualified SessionType
import SessionType (LocalAtom(sender, receiver, type_), LocalTypeState(participant))
import qualified Queue 
import Data.Thread as Thread
import Data.Context as Context 
import Data.PID as PID (PID, create, parent, child)
import Data.Expr

import Types
import qualified Cmd
import Debug.Trace as Debug

import GHC.Generics
import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Monoid ((<>))

type Participant = Identifier
type Queue = Queue.Queue
type QueueHistory = Queue.QueueHistory


init :: SessionType.GlobalType -> Map.Map Identifier (SessionType.LocalType String) -> Program -> ( Context Value, Thread History Program ) 
init globalType types program = 
    let 
        thread = Thread (PID.create [ 0 ]) [] [ program ] 
    in 
        ( Context.singleton globalType types thread, thread ) 

{-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
data Value 
    = Receive { channelName :: Identifier, creator :: PID } 
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
    | Sent { channelName :: Identifier, payload :: Identifier, creator :: PID }
    | Received { channelName :: Identifier, binding :: Identifier, payload :: Identifier, creator :: PID }
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread (Maybe Identifier) PID
    | BranchedOn BoolExpr Bool Program 
    | AssertedOn BoolExpr 
    deriving (Eq, Show, Generic, ElmType, ToJSON, FromJSON)


{-| The µOz Syntax -}            
data Program 
    = Sequence Program Program
    | Let Identifier Value Program
    | If BoolExpr Program Program
    | SpawnThread (Maybe Identifier) Program
    | Skip
    | Apply Identifier (List Identifier)
    | Send { channelName :: Identifier, payload :: Identifier, creator :: PID } 
    | Assert BoolExpr
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)


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

renameCreator :: PID -> Program -> Program
renameCreator new program = 
    case program of
        Sequence a b -> 
            Sequence (renameCreator new a) (renameCreator new b)

        Let name value continuation -> 
            Let name (renameCreatorInValue new value) (renameCreator new continuation)

        If condition trueBody falseBody -> 
            If condition (renameCreator new trueBody) (renameCreator new falseBody)

        SpawnThread name work ->
            SpawnThread name (renameCreator new work)

        Skip -> Skip

        Assert condition -> 
            Assert condition

        Apply f args -> 
            Apply f args 

        Send { channelName, payload } -> 
            Send channelName payload new


renameCreatorInValue :: PID -> Value -> Value 
renameCreatorInValue new value = 
    case value of
        VBool value -> 
            VBool value 

        VUnit ->
            VUnit

        Receive { channelName } -> 
            Receive channelName new

        Procedure arguments body -> 
            Procedure arguments $ renameCreator new body

        Port -> Port

        VInt value -> 
            VInt value 


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

        Send { channelName, payload, creator } -> 
            Send (tagger channelName) (tagger payload) creator


renameVariableInValue :: Identifier -> Identifier -> Value -> Value 
renameVariableInValue old new value = 
    case value of
        VBool value -> 
            VBool (renameBoolExpr old new value)

        VUnit ->
            VUnit

        Receive { channelName, creator } -> 
            Receive (if channelName == old then new else channelName) creator

        Procedure arguments body -> 
            if old `elem` arguments then
                value
            else
                Procedure arguments $ renameVariable old new body

        Port -> Port

        VInt value -> 
            VInt (renameIntExpr old new value)



-- ATOMIC STEPS for a single thread

{- messages are signals to the "global" state to influence other threads based on this thread's actions -} 

data BackwardMsg 
    = RollChild { caller :: PID, toRoll :: PID, typeName :: Maybe Identifier } 
    | RollSend { caller :: PID, histories :: List QueueHistory, channelName :: ChannelName } 
    | RollReceive { caller :: PID, histories :: List QueueHistory, channelName :: ChannelName } 
    deriving (Show)


newtype ForwardMsg
    = Spawn (Thread History Program) 
    deriving (Show)

{-| Take one step forward -} 
advanceP :: (MonadState (Context Value) m, MonadError Error m) 
    => PID 
    -> Program 
    -> List Program 
    -> m ( History, List Program, Cmd.Cmd ForwardMsg)
advanceP pid program rest = 
    let continue history rest = return ( history, rest, Cmd.none ) 
    in
        case program of
                Skip ->
                    continue Skipped rest 

                Sequence a b ->
                    continue Composed (a:b:rest)

                Let identifier value continuation -> 
                    case value of
                        Receive { channelName, creator } -> 
                            Context.modifyLocalTypeStateM creator $ \localTypeState -> 
                                case SessionType.forwardWithReceive localTypeState of
                                    Left e -> 
                                        error $ unwords [ show e, show pid, show program ] -- Except.throwError $ BlockedOnReceive pid

                                    Right ( expectedSender, valueType, newLocalTypeState ) -> do
                                        participant <- Context.lookupParticipant creator
                                        message_ <- readChannel pid expectedSender participant valueType channelName 
                                        case message_ of
                                            Nothing ->
                                                -- blocked on receive
                                                Except.throwError $ BlockedOnReceive pid

                                            Just message -> 
                                                return ( newLocalTypeState, 

                                                    ( Received{ channelName = channelName, binding = identifier, payload = message, creator = creator } 
                                                    , renameVariable identifier message continuation : rest
                                                    , Cmd.none 
                                                    ) )

                        Port -> do
                            channelName <- Context.insertChannel pid Queue.empty
                            continue (CreatedChannel channelName) $
                                renameVariable identifier channelName continuation : rest

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

                            continue (CreatedVariable functionName) $
                                renameVariable identifier functionName continuation : rest
                            

                        _ -> do
                            variableName <- Context.insertVariable pid value 
                            continue (CreatedVariable variableName) $
                                renameVariable identifier variableName continuation : rest


                If condition trueBody falseBody -> do
                    verdict <- evalBoolExpr condition 
                    if verdict then
                        continue (BranchedOn condition True falseBody) (trueBody : rest)
                    else
                        continue (BranchedOn condition False trueBody) (falseBody : rest)

                Assert condition -> do
                    verdict <- evalBoolExpr condition 
                    if not verdict then
                       Except.throwError $ AssertionError (show condition)
                    else
                        continue (AssertedOn condition) rest


                SpawnThread typeName work -> do
                    thread@(Thread threadName _ _) <- Context.insertThread pid [] [ work ] 

                    -- if there is a type name, insert this thread as a participant
                    mapM_ (Context.insertParticipant threadName) typeName 

                    return 
                        ( SpawnedThread typeName threadName
                        , rest
                        , Cmd.create $ Spawn thread
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
                            continue (CalledProcedure functionName arguments) (withRenamedVariables : rest)
                                
                Send { channelName, payload, creator } -> 
                    Context.modifyLocalTypeStateM creator $ \localTypeState -> 
                        case SessionType.forwardWithSend localTypeState of 
                            Right (expectedReceiver, valueType, newLocalTypeState) -> do
                                participant <- Context.lookupParticipant creator
                                writeChannel pid participant expectedReceiver valueType channelName payload 
                                return 
                                    ( newLocalTypeState
                                        , ( Sent channelName payload creator
                                    , rest 
                                    , Cmd.none 
                                          )
                                    )

                            Left e -> 
                                -- Except.throwError $ BlockedOnReceive pid
                                error "wants to send but session type won't allow it"

tracer f a = Debug.trace (f a) a

{-| Take one step backward -} 
backwardP :: (MonadState (Context Value) m, MonadError Error m) 
          => PID 
          -> History  
          -> List Program 
          -> m ( Bool, List Program, Cmd.Cmd BackwardMsg ) 
backwardP pid history program = do
    context <- State.get
    let continue newProgram = return ( True, newProgram, Cmd.none )
        f (h, p) =  unwords [ show pid , show h, show p ] ++ "\n\n"
    case tracer f (history, program) of 
                ( Skipped, _ ) ->
                    continue (Skip : program)

                ( Composed, first:second:restOfProgram ) ->
                    continue (Sequence first second : restOfProgram)

                ( CreatedVariable identifier, continuation : restOfProgram ) -> do
                    value <- Context.lookupVariable identifier 
                    State.modify $ removeVariable identifier 
                    continue (Let identifier value continuation : restOfProgram )

                ( CreatedChannel identifier, continuation : restOfProgram ) -> do
                    Context.removeChannel identifier
                    return ( True, Let identifier Port continuation : restOfProgram, Cmd.none )

                ( BranchedOn condition True falseBody, trueBody : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)
                    
                ( BranchedOn condition False trueBody, falseBody : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)

                ( AssertedOn condition, restOfProgram ) ->
                    continue (Assert condition : restOfProgram)

                ( CalledProcedure functionName arguments, body : restOfProgram ) ->
                    continue (Apply functionName arguments : restOfProgram)

                ( Sent { channelName, payload, creator }, restOfProgram ) -> 
                    Context.modifyLocalTypeStateM creator $ \localTypeState -> 
                        case SessionType.backwardWithSend localTypeState of
                            Left e -> 
                                error $ show e

                            Right ( receiver, newLocalTypeState ) -> 
                                withChannel channelName $ \queue -> do
                                    participant <- Context.lookupParticipant creator
                                    case Queue.rollPush pid participant receiver queue of
                                        Right (newChannel, _) -> do
                                            mapChannel channelName (const newChannel)

                                            return  ( newLocalTypeState,
                                                ( True
                                                , Send channelName payload creator : restOfProgram
                                                , Cmd.none 
                                                ))
                                        Left (Queue.InvalidAction _) -> do
                                            -- generate the list of actions on the channel that need to be undone
                                            -- including the current one: processing of the message will make us go
                                            -- into the `then` branch above.
                                            toRollFirst <- withChannel channelName (return . Queue.followingReceive pid)
                                            return 
                                                ( localTypeState
                                                    , ( False
                                                , restOfProgram
                                                , Cmd.create RollSend{ caller = pid, histories =  toRollFirst, channelName = channelName }
                                                ))

                                        Left e -> error $ "backward evaluation should not fail, but got" ++ show e


                ( Received{ channelName, binding, payload, creator }, continuation : restOfProgram ) -> 
                    Context.modifyLocalTypeStateM creator $ \localTypeState ->
                        case SessionType.backwardWithReceive localTypeState of
                            Left e -> 
                                error $ show e

                            Right ( sender, valueType, newLocalTypeState ) ->
                                withChannel channelName $ \queue -> do
                                    participant <- Context.lookupParticipant pid
                                    case Queue.rollPop pid participant sender valueType payload queue of
                                        Right newChannel -> do
                                            -- set the channel to the new version
                                            mapChannel channelName (const newChannel)

                                            return 
                                                ( newLocalTypeState
                                                    , ( True
                                                , Let binding (Receive channelName creator) (renameVariable payload binding continuation) : restOfProgram 
                                                , Cmd.none 
                                                ))

                                        Left (Queue.InvalidAction _) -> do
                                            -- generate the list of actions on the channel that need to be undone
                                            -- including the current one: processing of the message will make us go
                                            -- into the `then` branch above.
                                            toRollFirst <- withChannel channelName (return . Queue.followingReceive pid)

                                            return 
                                                ( localTypeState 
                                                    , ( False 
                                                , restOfProgram
                                                , Cmd.create RollReceive{ caller = pid, histories =  toRollFirst, channelName = channelName }
                                                ))

                                        Left e -> 
                                            error $ "backward evaluation should not fail, but got" ++ show e

                ( SpawnedThread typeName toRoll, restOfProgram ) -> 
                    return 
                        ( True 
                        , restOfProgram
                        , Cmd.create (RollChild pid toRoll typeName)
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
