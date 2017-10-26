{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module MicroOz where 


import qualified ReversibleLanguage (ReversibleLanguage(..))
import ReversibleLanguage (ReversibleLanguage(Value, History), Progress(..), Context(..), throw)
import Control.Monad.Trans.State as State 
import Control.Applicative (liftA2)
import Interpreter
import Types
import Queue
import qualified Data.Map as Map


instance ReversibleLanguage.ReversibleLanguage Program where 
    {-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
    data Value Program 
        = VTrue
        | VFalse
        | Receive Identifier
        | Procedure (List Identifier) Program
        | Port 
        | VInt IntExp
        deriving (Show, Eq)


         
    {-| Data type representing actions that have lead to the current program state -} 
    data History Program
        = Skipped
        | Composed 
        | Sent Identifier Identifier
        | Received Identifier Identifier
        | CreatedVariable Identifier
        | CreatedChannel Identifier
        | CalledProcedure Identifier (List Identifier)
        | SpawnedThread PID
        | BranchedOn BoolExp Bool Program 
        | AssertedOn BoolExp
        deriving (Eq, Show)

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



{-| The µOz Syntax -}            
data Program 
    = Sequence Program Program
    | Let Identifier (ReversibleLanguage.Value Program) Program
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

init :: Program -> (Context (Value Program),  Task (Thread Program))
init program = 
    ( Context (Map.singleton [0] 0) 0 Map.empty Map.empty
    , Parallel (Thread [0] [] [ program ]) []
    )


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


renameVariableInValue :: Identifier -> Identifier -> Value Program -> Value Program
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



lookupProcedure  :: Identifier -> Interpreter (Value Program) (List Identifier, Program) 
lookupProcedure identifier = do
    value <- lookupVariable identifier
    case value of
        Procedure arguments body -> 
            return (arguments, body)

        _ ->
            throw $ TypeError identifier "I expected a Procedure but instead got" (show value)


advance :: Thread Program -> Interpreter (Value Program) (Progress (Thread Program)) 
advance thread = 
    case thread of 
        Thread _ _ [] -> 
            -- the thread contains no further instructions
            return $ Done thread

        Thread name history (program : rest) -> 
            let 
                continue :: History Program -> List Program -> Interpreter (Value Program) (Progress (Thread Program)) 
                continue historyInstruction instructions = 
                    return $ Step (Thread name (historyInstruction : history) instructions)
            in
            case program of
                Skip ->
                    continue Skipped rest 

                Sequence a b ->
                    continue Composed (a:b:rest)

                Let identifier value continuation -> do
                    freshName <- freshIdentifier 
                    case value of
                        Receive channelName -> do 
                            message_ <- readChannel name channelName 
                            case message_ of
                                Nothing ->
                                    -- blocked on receive
                                    return $ Blocked $ Thread name history (program : rest)

                                Just message -> 
                                    continue (Received channelName freshName) $
                                        renameVariable identifier message continuation : rest

                        Port -> do
                            State.modify $ \context -> 
                                let 
                                    newChannels = Map.insert freshName Queue.empty (_channels context)
                                in
                                    context { _channels = newChannels } 

                            continue (CreatedChannel freshName) $
                                renameVariable identifier freshName continuation : rest

                        Procedure arguments body -> do
                            -- rename the function name itself in the body, for recursive functions
                            let renamedBody = 
                                    if identifier `notElem` arguments then
                                        renameVariable identifier freshName body
                                    else
                                        body

                            insertVariable freshName (Procedure arguments renamedBody) 
                            continue (CreatedVariable freshName) $
                                renameVariable identifier freshName continuation : rest
                            

                        _ -> do
                            insertVariable freshName value 
                            continue (CreatedVariable freshName) $
                                renameVariable identifier freshName continuation : rest


                If condition trueBody falseBody -> do
                    verdict <- evalBoolExp condition 
                    if verdict then
                        continue (BranchedOn condition True falseBody) (trueBody : rest)
                    else
                        continue (BranchedOn condition False trueBody) (falseBody : rest)

                Assert condition -> do
                    verdict <- evalBoolExp condition 
                    if not verdict then
                       throw $ AssertionError (show condition)
                    else
                        continue (AssertedOn condition) rest


                SpawnThread work -> do
                    threadName <- freshThreadName name
                    return $ Branched 
                        ( Thread name (SpawnedThread threadName : history) rest)
                        ( Thread threadName [] [ work ])

                Apply functionName arguments -> do
                    ( parameters, body ) <- lookupProcedure functionName 
                    if length arguments /= length parameters then
                        throw $ ArgumentMismatch functionName (length parameters) (length arguments) 
                    else 
                        let
                            withRenamedVariables = 
                                    foldr (uncurry renameVariable) body (zip parameters arguments)
                        in
                            continue (CalledProcedure functionName arguments) (withRenamedVariables : rest)
                                
                Send channelName variable -> do
                    writeChannel name channelName variable 
                    continue (Sent channelName variable) rest


{-| Move a thread one step backward -} 
rollback :: Thread Program -> Interpreter (Value Program) (ReversibleLanguage.Progress (Thread Program))
rollback thread@(Thread name history program) =
    case history of
        [] -> 
            -- do nothing
            return $ Done thread
        
        ( mostRecent : restOfHistory ) ->
            let 
                continue :: List Program -> Interpreter (Value Program) (ReversibleLanguage.Progress (Thread Program))
                continue = return . Step . Thread name restOfHistory 
            in
            case (mostRecent, program) of
                ( Skipped, restOfProgram ) ->
                    continue (Skip : program)

                ( Composed, first:second:restOfProgram ) ->
                    continue (Sequence first second : restOfProgram)

                ( CreatedVariable identifier, continuation : restOfProgram ) -> do
                    value <- lookupVariable identifier 
                    removeVariable identifier 
                    continue (Let identifier value continuation : restOfProgram )

                ( CreatedChannel identifier, continuation : restOfProgram ) -> do
                    State.modify $ \context -> 
                        context { _channels = Map.delete identifier (_channels context) 
                                , _variableCount = _variableCount context - 1
                                } 

                    continue (Let identifier Port continuation : restOfProgram )

                ( BranchedOn condition True falseBody, trueBody : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)
                    
                ( BranchedOn condition False trueBody, falseBody : restOfProgram ) ->
                    continue (If condition trueBody falseBody : restOfProgram)

                ( AssertedOn condition, restOfProgram ) ->
                    continue (Assert condition : restOfProgram)

                ( CalledProcedure functionName arguments, body : restOfProgram ) ->
                    continue (Apply functionName arguments : restOfProgram)

                ( Sent channelName valueName, restOfProgram ) -> do
                    -- reverse of send is receive
                    message <- readChannel name channelName 

                    continue $ Send channelName valueName : restOfProgram

                ( Received channelName valueName, continuation : restOfProgram ) -> do
                    -- reverse of receive is send
                    writeChannel name channelName valueName 

                    -- unbind the variable
                    removeVariable valueName 

                    continue $ Let valueName (Receive channelName) continuation : restOfProgram 

                ( _, restOfProgram) ->
                    -- the program has a pattern incompatible with the history instruction we're currently matching
                    error $ show mostRecent ++ " encountered a pattern it cannot match: " ++ show restOfProgram 



evalBoolExp :: BoolExp -> Interpreter (Value Program) Bool 
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


evalIntExp :: IntExp -> Interpreter (Value Program) Int 
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


evalIntValue :: IntValue -> Interpreter (Value Program) Int
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
                    throw $ TypeError reference "I expected an IntExpr but got" (show dereferenced)

             
evalBoolValue :: BoolValue -> Interpreter (Value Program) Bool
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
                    throw $ TypeError identifier "I expected a boolean value but got" (show other)
