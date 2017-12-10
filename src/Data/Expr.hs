{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, NamedFieldPuns, GADTs, StandaloneDeriving, DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, DefaultSignatures, OverloadedStrings #-}
module Data.Expr (IntExpr(..), BoolExpr(..), IntOperator(..), BooleanOperator(..), renameIntExpr, renameBoolExpr, intOperatorToFunction, boolOperatorToFunction, exprToIntExpr, exprToBoolExpr, Expr(..)) where

import Control.Monad.State
import Control.Monad.Except

import Types
import Data.Context (Context)

import Data.Aeson (ToJSON, FromJSON)
import Elm
import GHC.Generics


{-| Expressions as a generalized algebraic data type (GADT), allowing us
 to define expr once for any kind of type (Int and Bool for now). 
-}
data Expr a where
    Literal :: a -> Expr a
    Reference :: Identifier -> Expr a
    EBoolOperator :: BooleanOperator -> Expr Int -> Expr Int -> Expr Bool
    EIntOperator :: IntOperator -> Expr Int -> Expr Int -> Expr Int 

deriving instance Show a => Show (Expr a) 
deriving instance Eq a => Eq (Expr a) 

data IntExpr 
    = LiteralInt Int
    | ReferenceInt Identifier
    | IntOperator IntOperator IntExpr IntExpr 
    deriving (Show, Eq, Generic, ElmType, FromJSON, ToJSON)

intExprToExpr :: IntExpr -> Expr Int
intExprToExpr expr = 
    case expr of 
        LiteralInt v -> Literal v
        ReferenceInt r -> Reference r
        IntOperator op a b -> EIntOperator op (intExprToExpr a) (intExprToExpr b)

exprToIntExpr :: Expr Int -> IntExpr
exprToIntExpr expr = 
    case expr of 
        Literal v -> LiteralInt v
        Reference r -> ReferenceInt r 
        EIntOperator op a b -> IntOperator op (exprToIntExpr a) (exprToIntExpr b)


data BoolExpr 
    = LiteralBool Bool
    | ReferenceBool Identifier
    | BoolOperator BooleanOperator IntExpr IntExpr 
    deriving (Show, Eq, Generic, ElmType, FromJSON, ToJSON)


boolExprToExpr :: BoolExpr -> Expr Bool
boolExprToExpr expr = 
    case expr of 
        LiteralBool v -> Literal v
        ReferenceBool r -> Reference r
        BoolOperator op a b -> EBoolOperator op (intExprToExpr a) (intExprToExpr b)


exprToBoolExpr :: Expr Bool -> BoolExpr
exprToBoolExpr expr = 
    case expr of 
        Literal v -> LiteralBool v
        Reference r -> ReferenceBool r 
        EBoolOperator op a b -> BoolOperator op (exprToIntExpr a) (exprToIntExpr b)




data IntOperator = Add | Subtract | Divide | Multiply deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)


data BooleanOperator 
    = Equal 
    | LessThan
    | GreaterThan
    | LessThanEqual
    | GreaterThanEqual 
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)




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


renameExpr :: Identifier -> Identifier -> Expr a -> Expr a
renameExpr old new expression = 
    case expression of
        Literal v -> 
            Literal v

        Reference id -> 
            Reference (if id == old then new else id)

        EBoolOperator op left right -> 
            EBoolOperator op (renameExpr old new left) (renameExpr old new right)

        EIntOperator op left right -> 
            EIntOperator op (renameExpr old new left) (renameExpr old new right)

renameIntExpr old new = exprToIntExpr . renameExpr old new . intExprToExpr
renameBoolExpr old new = exprToBoolExpr . renameExpr old new . boolExprToExpr
