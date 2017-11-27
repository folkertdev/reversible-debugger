{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns  #-}
module MicroOz where 


import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Queue 
import Data.Thread as Thread
import Data.Context as Context 
import Data.ThreadState as ThreadState (Progress(..), ThreadState(..), OtherThreads(..), add, reschedule, rescheduleBackward, mapActive, mapOther, addInactive, addBlocked, addUninitialized, removeUninitialized)
import qualified Data.ThreadState
import Data.PID as PID (PID, create, parent, child)
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
        thread = Thread (PID.create [ 0 ]) [] [ program ] 
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

scheduleThread :: MonadError Error m => PID -> ThreadState h a -> m (ThreadState h a)
scheduleThread pid state = embedThreadScheduleError $ Data.ThreadState.scheduleThread pid state

scheduleThreadBackward :: MonadError Error m => PID -> ThreadState h a -> m (ThreadState h a)
scheduleThreadBackward pid state = embedThreadScheduleError $ Data.ThreadState.scheduleThreadBackward pid state

rollSends :: (MonadState (Context Value) m, MonadError Error m) => Int -> ChannelName -> ThreadState History Program -> m (ThreadState History Program) 
rollSends n channelName state = do
    histories <- Context.withChannel channelName (return . Queue.lastNSends n) 
    handleBackwardEffects (RollSend (PID.create []) histories) state


rollReceives :: (MonadState (Context Value) m, MonadError Error m) => Int -> ChannelName -> ThreadState History Program -> m (ThreadState History Program) 
rollReceives n channelName state = do
    histories <- Context.withChannel channelName (return . Queue.lastNReceives n) 
    handleBackwardEffects (RollSend (PID.create []) histories) state

data Caller = Parent PID | Self 

rollThread :: (MonadState (Context Value) m, MonadError Error m) => PID -> ThreadState History Program -> m (ThreadState History Program, Program) 
rollThread pid state = 
    scheduleThreadBackward pid state >>= \state -> 
        case state of
            Running current@(Thread _ _ program) rest -> do
                ( progress, cmd ) <- rollback current 

                let messages = Cmd.unpack cmd 

                case progress of
                    Done -> do
                        -- thread is completely unrolled, try scheduling the parent
                        rescheduledParent <- scheduleThreadBackward (PID.parent pid) (Stuck rest)
                        newState <- foldM (flip handleBackwardEffects) rescheduledParent messages 
                        Context.removeThread pid
                        case program of 
                            -- the initial program of a thread should be just 1 instruction
                            [ x ] -> 
                                return ( newState, x )
                            _ -> 
                                error $ "invalid initial program: " ++ show program


                    Step newCurrent -> do
                        newerCurrent <- foldM (flip handleBackwardEffects) (Running newCurrent rest) messages
                        rollThread pid newerCurrent

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
        scheduleThread pid state >>= \state -> 
            case state of 
                Running current rest -> do
                    ( progress, cmd ) <- rollback current 

                    let messages = Cmd.unpack cmd 

                    case progress of
                        Done -> 
                            foldM (flip handleBackwardEffects) (ThreadState.addUninitialized current $ Stuck rest) messages

                        Step newCurrent -> 
                            foldM (flip handleBackwardEffects) (Running newCurrent rest) messages

                            
    
                Stuck rest -> 
                    error "deadlock? "


handleBackwardEffects :: (MonadState (Context Value) m, MonadError Error m) => BackwardMsg -> ThreadState History Program -> m (ThreadState History Program)
handleBackwardEffects action state = 
    case action of
        RollThread { caller, toRoll }  -> do
            -- roll the thread, and recover its program
            ( newState, threadProgram ) <- rollThread toRoll state

            -- reschedule the parent
            newerState <- scheduleThreadBackward caller newState 

            -- remove child from uninitialized if it's in there
            let newererState = ThreadState.removeUninitialized toRoll newerState

            return $ flip ThreadState.mapActive newererState $ \(Thread pid history program)  -> 
                Thread pid history (SpawnThread threadProgram : program )

        Uninitialize { toUninitialize }  ->  
            let newState = scheduleThread toUninitialize state 

                remove state = 
                    case state of 
                        Running current other -> 
                            ThreadState.addUninitialized current (Stuck other)

                        Stuck other -> 
                            Stuck other 
            in
                fmap remove newState
        
        RollSend _ [] ->  
            return state

        RollSend caller (h:hs) -> do
            -- aquire pid 
            -- switch to that thread
            -- make it take a step back
            -- recurse (this will roll the previous thread until its channel action is rolled)
            stepBack <- rollChannel h state
            handleBackwardEffects (RollSend caller hs) stepBack

        RollReceive _ [] -> 
            return state

        RollReceive caller (h:hs) -> do
            stepBack <- rollChannel h state
            handleBackwardEffects (RollSend caller hs) stepBack

handleForwardEffects :: (MonadState (Context Value) m, MonadError Error m) => ForwardMsg -> ThreadState History Program -> m (ThreadState History Program)
handleForwardEffects action state = 
    case action of
        Spawn thread ->
            return $ ThreadState.add thread state


type ChannelName = Identifier 


data BackwardMsg 
    = Uninitialize { caller :: PID, toUninitialize :: PID }  
    | RollThread { caller :: PID, toRoll :: PID } 
    | RollSend { caller :: PID, history :: List QueueHistory } 
    | RollReceive { caller :: PID, history :: List QueueHistory } 
    deriving (Show)

newtype ForwardMsg
    = Spawn (Thread History Program) 
    deriving (Show)




debugLog name value = 
    Debug.traceShow (name ++ ": " ++ show value) value


backward :: (MonadState (Context Value) m, MonadError Error m) => ThreadState History Program -> m ( ThreadState History Program )
backward state = 
    case state of 
        Running current@(Thread pid _ _) rest -> do
            ( progress, commands ) <- rollback current

            let messages = Cmd.unpack commands

            case progress of
                Done ->
                    -- we re-add the current thread so it can be cleaned up 
                    -- properly (via the messages) 
                    foldM (flip handleBackwardEffects) (Running current rest)  messages

                Step newCurrent -> 
                    foldM (flip handleBackwardEffects) (Running newCurrent rest) messages


        Stuck rest ->
            case ThreadState.rescheduleBackward state  of 
                Just rescheduled -> 
                    backward rescheduled

                Nothing ->  
                    Except.throwError $ SchedulingError $ ThreadScheduleError (PID.create []) DeadLock


handleBlockedThread :: (MonadError Error m) => ThreadState h a -> Error -> m (ThreadState h a)
handleBlockedThread state error =  
    case error of 
        BlockedOnReceive pid -> 
            case state of 
                Running current rest -> 
                    Stuck rest
                        -- reschedule (makes sure the 'current' thread is not immediately rescheduled  
                        |> ThreadState.reschedule
                        -- insert the 'current' thread into the blocked other threads
                        |> fmap (return . ThreadState.addBlocked current)
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
                        foldM (flip handleForwardEffects) (ThreadState.addInactive current $ Stuck rest) messages

                    Step newCurrent -> 
                        foldM (flip handleForwardEffects) (Running newCurrent rest) messages


            Stuck rest ->
                case ThreadState.reschedule state  of 
                    Just rescheduled -> 
                        forward rescheduled

                    Nothing ->  
                        Except.throwError $ SchedulingError $ ThreadScheduleError (PID.create []) DeadLock


advance :: (MonadState (Context Value) m, MonadError Error m) => Thread History Program -> m ( Progress (Thread History Program), Cmd.Cmd ForwardMsg) 
advance thread@(Thread pid histories program) = 
    case program of
        [] -> 
            return ( Done, Cmd.none )

        (p:ps) -> do
            (h, newProgram, cmd ) <- advanceP pid p ps
            return ( Step $ Thread pid (h:histories) newProgram, cmd )


rollback :: (MonadState (Context Value) m, MonadError Error m) => Thread History Program -> m ( Progress (Thread History Program), Cmd.Cmd BackwardMsg)
rollback thread@(Thread pid histories program) = 
    case histories of
        [] -> 
            return ( Done , Cmd.create Uninitialize { caller = PID.parent pid, toUninitialize = pid })


        (h:hs) -> do
            ( newProgram, cmd ) <- backwardP pid h program 
            return ( Step $ Thread pid hs newProgram, cmd )

advanceP :: (MonadState (Context Value) m, MonadError Error m) => PID -> Program -> List Program -> m ( History, List Program, Cmd.Cmd ForwardMsg)
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
                                
                Send channelName variable -> do
                    writeChannel pid channelName variable 
                    continue (Sent channelName variable) rest


backwardP :: (MonadState (Context Value) m, MonadError Error m) => PID -> History -> List Program -> m ( List Program, Cmd.Cmd BackwardMsg ) 
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
                            , Cmd.create (RollSend pid toRollFirst)
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
                            , Cmd.create (RollReceive pid toRollFirst)
                            )

                ( SpawnedThread toRoll, restOfProgram ) -> 
                    return 
                        ( restOfProgram
                        , Cmd.create (RollThread pid toRoll)
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
