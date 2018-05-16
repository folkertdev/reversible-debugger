{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances  #-}
module Program where 

import Utils (List)

import Data.Fix
import qualified Data.List as List
import Control.Arrow (first, second) 

import Elm
import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=), (.:), object)
import GHC.Generics

import LocalType (Location, Participant, Identifier)


data ProgramF value next 
    = Send { owner :: Participant, value :: value, continuation :: next }
    | Receive { owner :: Participant, variableName :: Identifier, continuation :: next  }
    | Offer Participant (List (String, next))
    | Select Participant (List (String, value, next))
    | Parallel next next 
    | Application Participant Identifier value
    | Let Participant Identifier value next 
    | IfThenElse Participant value next next
    | Literal value -- needed to define multi-parameter functions
    | NoOp
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable, ToJSON, FromJSON, ElmType)

deriving instance (ElmType a, ElmType b, ElmType c) => ElmType (a,b,c)

type Program value = Fix (ProgramF value) 

terminate :: Program a
terminate = Fix NoOp

deriving instance (ToJSON (f (Fix f))) => ToJSON (Fix f)
deriving instance (FromJSON (f (Fix f))) => FromJSON (Fix f)
deriving instance (ElmType (f (Fix f))) => ElmType (Fix f)


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

        recursiveRename :: Program Value -> Program Value
        recursiveRename instruction = Fix $ 
            case unFix instruction of 
                Application owner functionName variableName ->
                    Application owner (rename functionName) (renameValue old new variableName)

                Let owner variableName value continuation ->  
                    if old == variableName then
                        -- variable shadowing: don't rename in the body
                        Let owner variableName (renameValue old new value) continuation 
                    else
                        Let owner variableName (renameValue old new value) (recursiveRename continuation)

                Receive owner variableName continuation -> 
                    if old == variableName then
                        -- variable shadowing: don't rename in the body
                        Receive owner variableName continuation 
                    else
                        Receive owner variableName (recursiveRename continuation)

                Select owner options -> 
                    Select owner $ List.map (\(label, condition, program) -> (label, renameValue old new condition, recursiveRename program)) options

                Send owner variable cont ->
                    Send owner (renameValue old new variable) (recursiveRename cont)

                Literal literal  ->
                    Literal (renameValue old new literal)

                IfThenElse owner condition thenBranch elseBranch -> 
                    IfThenElse owner (renameValue old new condition) (recursiveRename thenBranch) (recursiveRename elseBranch)

                Parallel a b -> 
                    Parallel (recursiveRename a) (recursiveRename b)

                Offer owner options -> 
                    Offer owner $ List.map (second recursiveRename) options

                NoOp -> 
                    NoOp

    in
        recursiveRename program
