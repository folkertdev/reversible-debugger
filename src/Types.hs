module Types where

import Control.Monad.Trans.Except(ExceptT(..), throwE, runExceptT, catchE)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM, liftM2)
import Control.Applicative (liftA2, ZipList(..)) 
import Control.Monad.Free

import Data.Traversable (sequence)


import Control.Concurrent.Chan
import qualified Data.Map as Map
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Data.IORef

type IOThrowsError a = ExceptT Error IO a

data Error
    = UndefinedVariable Identifier
    | UninitializedChannel Identifier
    | TypeError Identifier String Value
    | AssertionError BoolExp
    | BlockedOnReceive ThreadName 
    deriving (Show)

type List = []

newtype Identifier 
    = Identifier String 
    deriving (Eq, Show, Ord)

newtype ThreadName  
    = ThreadName String 
    deriving (Eq, Show, Ord)


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
    | Esc
    deriving (Show, Eq)

         
{-| Data type representing actions that have lead to the current program state -} 
data History 
    = Skipped
    | Composed 
    | Sent Identifier
    | Received Identifier Identifier
    | CreatedVariable Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread ThreadName
    | BranchedOn BoolExp Bool Program 
    | AssertedOn BoolExp
    | HistoryEsc
    deriving (Show, Eq)


{-| Values in the language. These may appear on the right-hand side of a variable declaration -} 
data Value
    = VTrue
    | VFalse
    | Receive Identifier
    | Procedure (List Identifier) Program 
    | Port (Maybe (Chan Identifier))
    | VInt IntExp
    deriving (Show, Eq)

instance Show (Chan a) where show _ = "Channel"



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


renameVariableInIntExp old new intExp = 
    let 
        renameIntValue value =
            case value of
                IntValue v -> 
                    IntValue v

                IntIdentifier id ->
                    if id == old then
                        IntIdentifier new
                    else
                        value
    in
        case intExp of
            AtomInt value -> AtomInt (renameIntValue value)
            Add value expr -> Add (renameIntValue value) (renameVariableInIntExp old new expr)
            Subtract value expr -> Subtract (renameIntValue value) (renameVariableInIntExp old new expr)
            Divide value expr -> Divide (renameIntValue value) (renameVariableInIntExp old new expr)
            Multiply value expr -> Multiply (renameIntValue value) (renameVariableInIntExp old new expr)


data IntValue 
    = IntValue Int 
    | IntIdentifier Identifier
    deriving (Show, Eq)


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
            If condition (renameVariable old new trueBody) (renameVariable old new falseBody)

        SpawnThread work ->
            SpawnThread (renameVariable old new work)

        Skip -> Skip


        Assert condition -> Assert condition

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

        Port v -> Port v

        VInt ie -> 
            VInt $ renameVariableInIntExp old new ie 


