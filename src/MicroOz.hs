{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns, GADTs, StandaloneDeriving #-}
module MicroOz 
    (Program(..)
    , History(..)
    , Value(..)
    , ForwardMsg(..)
    , BackwardMsg(..)
    , advanceP
    , backwardP
    , MicroOz.init
    , Expr(..)
    , IntOperator(..)
    , BooleanOperator(..)
    ) where

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Applicative (liftA2)

import qualified Queue 
import Data.Thread as Thread
import Data.Context as Context 
import Data.PID as PID (PID, create, parent, child)

import Types
import qualified Cmd

type Queue = Queue.Queue
type QueueHistory = Queue.QueueHistory


init :: Program -> ( Context Value, Thread History Program ) 
init program = 
    let 
        thread = Thread (PID.create [ 0 ]) [] [ program ] 
    in 
        ( Context.singleton thread, thread ) 

{-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
data Value 
    = Receive Identifier
    | Procedure (List Identifier) Program
    | Port 
    | VInt (Expr Int) 
    | VBool (Expr Bool)
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
    | BranchedOn (Expr Bool) Bool Program 
    | AssertedOn (Expr Bool) 
    deriving (Eq, Show)


{-| The µOz Syntax -}            
data Program 
    = Sequence Program Program
    | Let Identifier Value Program
    | If (Expr Bool) Program Program
    | SpawnThread Program
    | Skip
    | Apply Identifier (List Identifier)
    | Send Identifier Identifier
    | Assert (Expr Bool)
    deriving (Show, Eq)

-- EXPR 


{-| Expressions as a generalized algebraic data type (GADT), allowing us
 to define expr once for any kind of type (Int and Bool for now). 
-}
data Expr a where
    Literal :: a -> Expr a
    Reference :: Identifier -> Expr a
    BoolOperator :: BooleanOperator -> Expr Int -> Expr Int -> Expr Bool
    IntOperator :: IntOperator -> Expr Int -> Expr Int -> Expr Int 

deriving instance Show a => Show (Expr a) 
deriving instance Eq a => Eq (Expr a) 


data IntOperator = Add | Subtract | Divide | Multiply deriving (Show, Eq)


data BooleanOperator 
    = Equal 
    | LessThan
    | GreaterThan
    | LessThanEqual
    | GreaterThanEqual 
    deriving (Show, Eq)



evalIntExpr :: (MonadState (Context Value) m, MonadError Error m) => Expr Int -> m Int
evalIntExpr expression =
    case expression of
        Literal value -> 
            return value 

        IntOperator op left right ->
            liftA2 (intOperatorToFunction op) (evalIntExpr left) (evalIntExpr right) 

        Reference identifier -> do
            value <- lookupVariable identifier 
            case value of
                VInt value -> 
                    evalIntExpr value

                other -> 
                    Except.throwError $ TypeError identifier "I expected a integer value but got" (show other)


evalBoolExpr :: (MonadState (Context Value) m, MonadError Error m) => Expr Bool -> m Bool
evalBoolExpr expression =
    case expression of
        Literal value -> 
            return value 

        BoolOperator op left right ->
            liftA2 (boolOperatorToFunction op) (evalIntExpr left) (evalIntExpr right) 

        Reference identifier -> do
            value <- lookupVariable identifier 
            case value of
                VBool value -> 
                    evalBoolExpr value

                other -> 
                    Except.throwError $ TypeError identifier "I expected a boolean value but got" (show other)


intOperatorToFunction :: IntOperator -> (Int -> Int -> Int) 
intOperatorToFunction operator =
    case operator of
        Add -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Divide -> div 


boolOperatorToFunction operator =
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
            If (renameExpr old new condition) (renameVariable old new trueBody) (renameVariable old new falseBody)

        SpawnThread work ->
            SpawnThread (renameVariable old new work)

        Skip -> Skip

        Assert condition -> Assert (renameExpr old new condition)

        Apply f args -> 
            Apply (tagger f) (map tagger args)

        Send channelName contents -> 
            Send (tagger channelName) (tagger contents)


renameVariableInValue :: Identifier -> Identifier -> Value -> Value 
renameVariableInValue old new value = 
    case value of
        VBool value -> 
            VBool (renameExpr old new value)

        Receive identifier -> 
            Receive (if identifier == old then new else identifier)

        Procedure arguments body -> 
            if old `elem` arguments then
                value
            else
                Procedure arguments $ renameVariable old new body

        Port -> Port

        VInt value -> 
            VInt (renameExpr old new value)


renameExpr :: Identifier -> Identifier -> Expr a -> Expr a
renameExpr old new expression = 
    case expression of
        Literal v -> 
            Literal v

        Reference id -> 
            Reference (if id == old then new else id)

        BoolOperator op left right -> 
            BoolOperator op (renameExpr old new left) (renameExpr old new right)

        IntOperator op left right -> 
            IntOperator op (renameExpr old new left) (renameExpr old new right)


-- ATOMIC STEPS for a single thread

{- messages are signals to the "global" state to influence other threads based on this thread's actions -} 

data BackwardMsg 
    = RollChild { caller :: PID, toRoll :: PID } 
    | RollSend { caller :: PID, history :: List QueueHistory } 
    | RollReceive { caller :: PID, history :: List QueueHistory } 
    deriving (Show)


newtype ForwardMsg
    = Spawn (Thread History Program) 
    deriving (Show)

{-| Take one step forward -} 
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
                            case message_ of
                                Nothing ->
                                    -- blocked on receive
                                    Except.throwError $ BlockedOnReceive pid

                                Just message -> 
                                    continue Received{ channelName = channelName, binding = identifier, payload = message } $
                                        renameVariable identifier message continuation : rest

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


{-| Take one step backward -} 
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
                        -- generate the list of actions on the channel that need to be undone
                        -- including the current one: processing of the message will make us go
                        -- into the `then` branch above.
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
                        -- generate the list of actions on the channel that need to be undone
                        -- including the current one: processing of the message will make us go
                        -- into the `then` branch above.
                        toRollFirst <- withChannel channelName (return . Queue.followingReceive pid)
                        return 
                            ( restOfProgram
                            , Cmd.create (RollReceive pid toRollFirst)
                            )

                ( SpawnedThread toRoll, restOfProgram ) -> 
                    return 
                        ( restOfProgram
                        , Cmd.create (RollChild pid toRoll)
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
