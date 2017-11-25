{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns #-}
module MicroOz where 

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Queue 
import Data.Thread as Thread
import Data.Context as Context 
import Data.ThreadState as ThreadState (Progress(..), ThreadState(..), OtherThreads(..), add, reschedule, scheduleThread, schedule, mapActive, mapOther)
import Types
import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Applicative (liftA2)
import qualified Cmd
import Debug.Trace as Debug

type Map = Map.Map
type Queue = Queue.Queue
type QueueHistory = Queue.QueueHistory

throw :: Error -> StateT s (Either Error) a
throw error = State.StateT (\s -> Left error)

init :: Program -> ( Context Value, Thread History Program ) 
init program = 
    let 
        thread = Thread [ 0 ] [] [ program ] 
    in 
        ( Context.singleton thread, thread ) 

{-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
data Value 
    = VTrue
    | VFalse
    | Receive Identifier
    | Procedure (List Identifier) Program
    | Port 
    | VInt IntExp
    deriving (Show, Eq)


{-| Data type representing actions that have lead to the current program state -} 
data History 
    = Skipped
    | Composed 
    | Sent Identifier Identifier
    | Received { channelName :: Identifier, binding :: Identifier, payload :: Identifier }
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread PID
    | BranchedOn BoolExp Bool Program 
    | AssertedOn BoolExp
    deriving (Eq, Show)

{-

import qualified ReversibleLanguage (ReversibleLanguage(..))
import Control.Monad.Trans.State as State 
import Control.Applicative (liftA2)
import Interpreter
import Types
import Queue
import qualified Data.Map as Map


instance ReversibleLanguage.ReversibleLanguage Program where 


         
    forwardThread = advance
    backwardThread = rollback

    spawn = SpawnThread

    createdVariable history =
        case history of
            CreatedVariable name -> Just name
            _ -> Nothing  

    spawned history = 
        case history of
            SpawnedThread name -> 
                Just name

            _ -> 
                Nothing

    sent history =
        case history of 
            Sent channelName _ -> 
                Just channelName

            _ -> 
                Nothing

    received history = 
        case history of 
            Received channelName _ -> 
                Just channelName

            _ -> 
                Nothing

-}


{-| The µOz Syntax -}            
data Program 
    = Sequence Program Program
    | Let Identifier Value Program
    | If BoolExp Program Program
    | SpawnThread Program
    | Skip
    | Apply Identifier (List Identifier)
    | Send Identifier Identifier
    | Assert BoolExp
    deriving (Show, Eq)


data BoolValue = BoolValue Bool | BoolIdentifier Identifier deriving (Show, Eq) 


data BooleanOperator 
    = Equal 
    | LessThan
    | GreaterThan
    | LessThanEqual
    | GreaterThanEqual 
    deriving (Show, Eq)

data BoolExp 
    = AtomBool BoolValue
    | Operator BooleanOperator IntValue IntValue
    deriving (Show, Eq)


data IntExp 
    = AtomInt IntValue
    | Add IntValue IntExp
    | Subtract IntValue IntExp
    | Divide IntValue IntExp
    | Multiply IntValue IntExp
    deriving (Show, Eq)

data IntValue 
    = IntValue Int 
    | IntIdentifier Identifier
    deriving (Show, Eq)

{-
init :: Program -> (Context (Value Program), Thread Program) 
init program = 
    ( Context (Map.singleton [0] 0) 0 Map.empty Map.empty
    , Thread [0] [] [ program ]
    )
-}


renameVariableInBoolExp old new exp = 
    case exp of 
        AtomBool v@(BoolValue _) -> AtomBool v
        AtomBool (BoolIdentifier ident) -> AtomBool $ BoolIdentifier $ if ident == old then new else ident
        Operator op left right ->
            Operator op (renameIntValue old new left) (renameIntValue old new right)
    



renameIntValue old new value =
            case value of
                IntValue v -> 
                    IntValue v

                IntIdentifier id ->
                    if id == old then
                        IntIdentifier new
                    else
                        value

renameVariableInIntExp old new intExp = 
        case intExp of
            AtomInt value -> AtomInt (renameIntValue old new value)
            Add value expr -> Add (renameIntValue old new value) (renameVariableInIntExp old new expr)
            Subtract value expr -> Subtract (renameIntValue old new value) (renameVariableInIntExp old new expr)
            Divide value expr -> Divide (renameIntValue old new value) (renameVariableInIntExp old new expr)
            Multiply value expr -> Multiply (renameIntValue old new value) (renameVariableInIntExp old new expr)







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
            If (renameVariableInBoolExp old new condition) (renameVariable old new trueBody) (renameVariable old new falseBody)

        SpawnThread work ->
            SpawnThread (renameVariable old new work)

        Skip -> Skip

        Assert condition -> Assert (renameVariableInBoolExp old new condition)

        Apply f args -> 
            Apply (tagger f) (map tagger args)

        Send channelName contents -> 
            Send (tagger channelName) (tagger contents)


renameVariableInValue :: Identifier -> Identifier -> Value -> Value 
renameVariableInValue old new value = 
    case value of
        VTrue -> VTrue
        VFalse -> VFalse
        Receive identifier -> 
            if identifier == old then
                Receive new
            else
                value

        Procedure arguments body -> 
            if old `elem` arguments then
                value
            else
                Procedure arguments $ renameVariable old new body

        Port -> Port

        VInt ie -> 
            VInt $ renameVariableInIntExp old new ie 


-- lookupProcedure  :: Identifier -> Interpreter (Value Program) (List Identifier, Program) 
lookupProcedure :: (MonadState (Context Value) m, MonadError Error m) => Identifier -> m (List Identifier, Program)
lookupProcedure identifier = do
    value <- lookupVariable identifier 
    case value of
        Procedure arguments body -> 
            return (arguments, body)

        _ ->
            Except.throwError $ TypeError identifier "I expected a Procedure but instead got" (show value)


embedThreadScheduleError :: MonadError Error m =>  Either ThreadScheduleError a -> m a
embedThreadScheduleError value = 
    case value of 
        Left e ->
            Except.throwError $ SchedulingError e

        Right v ->
            return v


rollThread :: (MonadState (Context Value) m, MonadError Error m) => PID -> ThreadState History Program -> m (ThreadState History Program) 
rollThread pid state = 
    embedThreadScheduleError (scheduleThread pid state) >>= \state -> 
        case state of
            Running current rest -> do
                ( progress, cmd ) <- rollback current 

                let messages = Cmd.unpack cmd 

                case progress of
                    Done -> do
                        -- thread is completely unrolled, try scheduling the parent
                        rescheduledParent <- embedThreadScheduleError (scheduleThread (List.init pid) $ Debug.traceShowId $ Stuck rest) 
                        newState <- foldM (flip handleSideEffects) rescheduledParent messages
                        Context.removeThread pid
                        return newState


                    Step newCurrent -> do
                        newerCurrent <- foldM (flip handleSideEffects) (Running newCurrent rest) messages
                        rollThread pid newerCurrent

                    Blocked _ -> 
                        error "backwards move cannot block"
            Stuck _ -> 
                -- scheduleThread will have errored
                undefined


        
        
        


                        
rollChannel :: (MonadState (Context Value) m, MonadError Error m) => QueueHistory -> ThreadState History Program -> m (ThreadState History Program)
rollChannel history state = 
    let pid = 
            case history of 
                Queue.Added v -> v
                Queue.Removed v -> v
    in 
        embedThreadScheduleError (scheduleThread pid state) >>= \state -> 
            case state of 
                Running current rest -> do
                    ( progress, cmd ) <- rollback current 

                    let messages = Cmd.unpack cmd 

                    case progress of
                        Done -> 
                            foldM (flip handleSideEffects) (Stuck $ rest { inactive = Map.insert pid current (inactive rest) }) messages

                        Step newCurrent -> 
                            foldM (flip handleSideEffects) (Running newCurrent rest) messages

                        Blocked _ -> 
                            error "backwards move cannot block"
                            
    
                Stuck rest -> 
                    error "deadlock? "









handleSideEffects :: (MonadState (Context Value) m, MonadError Error m) => Msg -> ThreadState History Program -> m (ThreadState History Program)
handleSideEffects (Action caller action) state =  
    case action of
        Spawn thread ->
            return $ ThreadState.add thread state

        RollThread threadToRoll -> 
            rollThread threadToRoll state

        RollVariable _ -> undefined
        
        RollSend [] ->  
            return state

        RollSend (h:hs) -> do
            -- aquire pid 
            -- switch to that thread
            -- make it take a step back
            -- recurse (this will roll the previous thread until its channel action is rolled)
            stepBack <- rollChannel h state
            handleSideEffects (Action caller $ RollSend hs) stepBack

            

        RollReceive [] -> 
            return state

        RollReceive (h:hs) -> do
            stepBack <- rollChannel h state
            handleSideEffects (Action caller $ RollSend hs) stepBack



-- backward side effects: things other threads should do 
data Msg = Action PID Action 

type ChannelName = Identifier 
data Action = RollThread PID | RollVariable Identifier | RollSend (List QueueHistory) | RollReceive (List QueueHistory) | Spawn (Thread History Program)

backward :: (MonadState (Context Value) m, MonadError Error m) => ThreadState History Program -> m ( ThreadState History Program )
backward state = 
    case state of 
        Running current@(Thread pid _ _) rest -> do
            ( progress, commands ) <- rollback current

            let messages = Cmd.unpack commands

            case progress of
                Done ->
                    foldM (flip handleSideEffects) (Stuck rest) messages

                Step newCurrent -> 
                    foldM (flip handleSideEffects) (Running newCurrent rest) messages

                Blocked _ -> 
                    error "blocked on backward action"

        Stuck rest ->
            return $ Stuck rest


handleBlockedThread :: (MonadError Error m) => ThreadState h a -> Error -> m (ThreadState h a)
handleBlockedThread state error =  
    let insertBlocked thread other = 
            other { blocked = Map.insert (Thread.pid thread) thread (blocked other) } 
    in
    case error of 
        BlockedOnReceive pid -> 
            case state of 
                Running current rest -> 
                    Stuck rest
                        -- reschedule (makes sure the 'current' thread is not immediately rescheduled  
                        |> ThreadState.reschedule
                        -- insert the 'current' thread into the blocked other threads
                        |> fmap (return . ThreadState.mapOther (insertBlocked current))
                        -- if rescheduling failed, propagate the error 
                        |> Maybe.fromMaybe (Except.throwError (BlockedOnReceive pid))

                -- should not happen really
                Stuck rest -> return $ Stuck rest

        _ ->
            -- propagate any other errors
            Except.throwError error 


forward :: (MonadState (Context Value) m, MonadError Error m) => ThreadState History Program -> m ( ThreadState History Program )
forward state = 
    flip Except.catchError (handleBlockedThread state) $ 
        case state of 
            Running current@(Thread pid _ _) rest -> do
                ( progress, commands ) <- advance current 

                let messages = Cmd.unpack commands

                case progress of
                    Done -> 
                        foldM (flip handleSideEffects) (Stuck $ rest { inactive = Map.insert pid current (inactive rest) }) messages

                    Step newCurrent -> 
                        foldM (flip handleSideEffects) (Running newCurrent rest) messages

                    Blocked _ -> 
                        error "blocked on forward action"

            Stuck rest ->
                case ThreadState.reschedule state  of 
                    Just rescheduled -> 
                        forward rescheduled

                    Nothing ->  
                        Except.throwError $ SchedulingError $ ThreadScheduleError [] DeadLock


advance :: (MonadState (Context Value) m, MonadError Error m) => Thread History Program -> m ( Progress (Thread History Program), Cmd.Cmd Msg)
advance thread@(Thread pid histories program) = 
    case program of
        [] -> 
            return ( Done, Cmd.none )

        (p:ps) -> do
            (h, newProgram, cmd ) <- advanceP pid p ps
            return ( Step $ Thread pid (h:histories) newProgram, cmd )


rollback :: (MonadState (Context Value) m, MonadError Error m) => Thread History Program -> m ( Progress (Thread History Program), Cmd.Cmd Msg)
rollback thread@(Thread pid histories program) = 
    case histories of
        [] -> 
            return ( Done, Cmd.none )

        (h:hs) -> do
            ( newProgram, cmd ) <- backwardP pid h program 
            return ( Step $ Thread pid hs newProgram, cmd )

advanceP :: (MonadState (Context Value) m, MonadError Error m) => PID -> Program -> List Program -> m ( History, List Program, Cmd.Cmd Msg)
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
                        Receive channelName -> do 
                            message_ <- readChannel pid channelName 
                            case Debug.traceShow "receiving message from channel" message_ of
                                Nothing ->
                                    -- blocked on receive
                                    Except.throwError $ BlockedOnReceive pid

                                Just message -> 
                                    continue Received{ channelName = channelName, binding = identifier, payload = message } $
                                        renameVariable identifier message continuation : rest

                        Port -> do
                            channelName <- Context.insertChannel Queue.empty
                            continue (CreatedChannel channelName) $
                                renameVariable identifier channelName continuation : rest

                        Procedure arguments body -> do
                            functionName <- Context.insertBinding $ \functionName -> 
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
                            variableName <- Context.insertVariable value 
                            continue (CreatedVariable variableName) $
                                renameVariable identifier variableName continuation : rest


                If condition trueBody falseBody -> do
                    verdict <- evalBoolExp condition 
                    if verdict then
                        continue (BranchedOn condition True falseBody) (trueBody : rest)
                    else
                        continue (BranchedOn condition False trueBody) (falseBody : rest)

                Assert condition -> do
                    verdict <- evalBoolExp condition 
                    if not verdict then
                       Except.throwError $ AssertionError (show condition)
                    else
                        continue (AssertedOn condition) rest


                SpawnThread work -> do
                    thread@(Thread threadName _ _) <- Context.insertThread pid [] [ work ] 
                    return 
                        ( SpawnedThread threadName
                        , rest
                        , Cmd.create $ Action pid (Spawn thread )
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
                                
                Send channelName variable -> do
                    writeChannel pid channelName variable 
                    continue (Sent channelName variable) rest


backwardP :: (MonadState (Context Value) m, MonadError Error m) => PID -> History -> List Program -> m ( List Program, Cmd.Cmd Msg ) 
backwardP pid history program = do
    context <- State.get
    let continue newProgram = return ( newProgram, Cmd.none )
    case (history, program) of 
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
                    return ( Let identifier Port continuation : restOfProgram, Cmd.none )

                ( BranchedOn condition True falseBody, trueBody : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)
                    
                ( BranchedOn condition False trueBody, falseBody : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)

                ( AssertedOn condition, restOfProgram ) ->
                    continue (Assert condition : restOfProgram)

                ( CalledProcedure functionName arguments, body : restOfProgram ) ->
                    continue (Apply functionName arguments : restOfProgram)

                ( Sent channelName valueName, restOfProgram ) -> do
                    -- find out whether this send is the "latest" command on the channel
                    mostRecent <- withChannel channelName (return . Queue.hasJustSent pid)  
                    if mostRecent then do
                        -- remove value from the queue
                        mapChannel channelName (fst . Queue.unpush)

                        -- reconstruct the send instruction
                        return 
                            ( Send channelName valueName : restOfProgram
                            , Cmd.none 
                            )
                    else do 
                        toRollFirst <- withChannel channelName (return . Queue.followingReceive pid)
                        return 
                            ( restOfProgram
                            , Cmd.create (Action pid (RollSend  toRollFirst ))
                            )

                ( Received{ channelName, binding, payload }, continuation : restOfProgram ) -> do
                    -- find out whether this receive is the "latest" command on the channel
                    mostRecent <- withChannel channelName (return . Queue.hasJustReceived pid)  
                    if mostRecent then do
                        -- put the received value back onto the queue
                        mapChannel channelName $ \channel -> Queue.unpop payload channel

                        -- reconstruct the receive instruction
                        return 
                            ( Let binding (Receive channelName) (renameVariable payload binding continuation) : restOfProgram 
                            , Cmd.none 
                            )
                    else do 
                        toRollFirst <- withChannel channelName (return . Queue.followingReceive pid)
                        return 
                            ( restOfProgram
                            , Cmd.create (Action pid (RollReceive toRollFirst ))
                            )

                ( SpawnedThread toRoll, restOfProgram ) -> 
                    return 
                        ( restOfProgram
                        , Cmd.create (Action pid (RollThread toRoll))
                        )

                ( _, restOfProgram) ->
                    -- the program has a pattern incompatible with the history instruction we're currently matching
                    error $ show history ++ " encountered a pattern it cannot match: " ++ show restOfProgram 


evalBoolExp :: (MonadState (Context Value) m, MonadError Error m) => BoolExp -> m Bool 
evalBoolExp expression = 
    let
        toFunction operator =
            case operator of
                Equal -> 
                    (==)

                LessThan -> 
                    (<)

                GreaterThan -> 
                    (>)

                LessThanEqual -> 
                    (<=)

                GreaterThanEqual  -> 
                    (>=)
    in
        case expression of 
            AtomBool bool -> 
                evalBoolValue bool

            Operator op a b -> 
                liftA2 (toFunction op) (evalIntValue a) (evalIntValue b)


evalIntExp :: (MonadState (Context Value) m, MonadError Error m) => IntExp -> m Int 
evalIntExp expression = 
    let evalOperator f a b = do
            x <- evalIntValue  a
            y <- evalIntExp  b
            return $ f x y
    in
        case expression of 
            AtomInt int -> 
                evalIntValue int 

            Add a b ->
                evalOperator (+) a b

            Subtract a b ->
                evalOperator (-) a b

            Multiply a b ->
                evalOperator (*) a b

            Divide a b ->
                evalOperator div a b


evalIntValue :: (MonadState (Context Value) m, MonadError Error m) => IntValue -> m Int 
evalIntValue value =
    case value of
        IntValue int -> 
            return int

        IntIdentifier reference -> do
            dereferenced <- lookupVariable reference 
            case dereferenced of
                VInt intExpr ->
                    evalIntExp intExpr

                _ ->
                    Except.throwError $ TypeError reference "I expected an IntExpr but got" (show dereferenced)


             
evalBoolValue :: (MonadState (Context Value) m, MonadError Error m) => BoolValue -> m Bool 
evalBoolValue value = 
    case value of
        BoolValue bool -> 
            return bool

        BoolIdentifier identifier -> do
            value <- lookupVariable identifier 
            case value of 
                VTrue -> 
                    return True

                VFalse ->
                    return False

                other -> 
                    Except.throwError $ TypeError identifier "I expected a boolean value but got" (show other)
