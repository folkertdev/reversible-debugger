{-# LANGUAGE NamedFieldPuns #-}
module MicroOz.Parser (program, programWithTypes) where

import Control.Applicative ((<*), liftA2, pure)
import Data.List (intercalate)

import Text.ParserCombinators.Parsec as Parsec

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Types (List, Identifier(..))
import MicroOz
import Queue
import qualified SessionType
import SessionType (GlobalType(..))
import Data.PID as PID
import Data.Expr (Expr(..), IntExpr(..), BoolExpr(..), exprToIntExpr, exprToBoolExpr)

program :: String -> Either ParseError Program
program rawString = do 
    withoutWhitespace <- Parsec.parse eatComments "" rawString
    Parsec.parse programParser "" withoutWhitespace


programWithTypes :: String -> Either ParseError (Map.Map Identifier (SessionType.LocalType String), Program)
programWithTypes rawString = do 
    withoutWhitespace <- Parsec.parse eatComments "" rawString

    let parser = do
            globals <- globalType `sepBy` spaces 
            spaces
            (globalAtoms, program ) <- sessionWhere (Map.fromList globals)
            return (SessionType.deriveLocals globalAtoms, program )

    Parsec.parse parser "" withoutWhitespace

--- Parser
--
whitespaceOrComment = 
    Parsec.spaces

comment :: GenParser Char st ()
comment =
    (string "//" >> manyTill anyChar newline >> spaces >> return ()) <|>
    (string "/*" >> manyTill anyChar (string "*/") >>  spaces >> return ())

notComment = manyTill anyChar (lookAhead (comment <|> eof))

eatComments :: GenParser Char st String
eatComments = do
  optional comment
  xs <- sepBy notComment comment
  optional comment
  return $ intercalate "" xs

-- SESSION TYPES 


globalType :: Parser ( Identifier, SessionType.GlobalType )
globalType = do
    try $ string "global type"
    spaces
    name <- identifierParser
    arguments <- identifierParser `sepBy` spaces 
    spaces
    char '='
    spaces
    tipe <- SessionType.globalType arguments
    return ( name, tipe )

typeAlias :: Parser (Identifier, (Identifier, Identifier)) 
typeAlias = do
    try $ string "type alias"
    spaces
    name <- identifierParser
    char '='
    spaces
    tipe <- typeAt 
    return ( name, tipe )

typeAt = do
    global <- identifierParser
    string "at"
    spaces
    local <- identifierParser
    return (global, local )


sessionWhere :: Map.Map Identifier GlobalType -> Parser ( List SessionType.GlobalAtom, Program )
sessionWhere globals = do
    try $ string "session"
    spaces
    typeName <- identifierParser 
    case Map.lookup typeName globals of
        Nothing ->
            unexpected $ "I'm trying to instantiate session type `" ++ show typeName ++ "`, but it is not defined" 
        Just global@GlobalType{ parameters, atoms } -> 
            let header = do
                    arguments <- count (length parameters) identifierParser 
                    spaces
                    string "where"
                    return arguments
            in do
                arguments <- try header
                program <- programParser
                end
                return ( SessionType.instanciateGlobalType arguments global, program )
                
    
-- Program Structure


eol :: GenParser Char st Char
eol = char '\n'

letBinding :: Parser Program
letBinding = do
    try $ do 
        string "let"
        whitespaceOrComment 
    name <- identifierParser
    whitespaceOrComment 
    char '='
    whitespaceOrComment 
    value <- valueParser
    whitespaceOrComment 
    string "in"
    whitespaceOrComment 
    continuation <- programParser
    end
    return $ Let name value continuation

ifThenElse = do
    try $ do
        string "if"
        whitespaceOrComment
    condition <- boolExprParser
    whitespaceOrComment
    string "then"
    whitespaceOrComment
    trueBody <- programParser
    whitespaceOrComment
    string "else"
    whitespaceOrComment
    falseBody <- programParser
    end
    return $ If (exprToBoolExpr condition) trueBody falseBody

assertParser = do
    try $ string "assert"
    whitespaceOrComment
    condition <- boolExprParser
    whitespaceOrComment
    return $ Assert (exprToBoolExpr condition)


programParser = do
    whitespaceOrComment
    statements <- parseStatements
    case statements of 
        [] -> error "no statements parsed, but the parser succeeded?" 
        (x:xs) -> return $ foldl Sequence x xs




parseStatements = do
    leftArgument <- choice 
        [ skipParser  
        , letBinding
        , threadParser
        , sendParser
        , functionApplicationParser
        , ifThenElse
        , assertParser
        ] 
    whitespaceOrComment 

    let recursive = do
            whitespaceOrComment
            char ';'
            whitespaceOrComment
            rest <- parseStatements
            return $ leftArgument : rest


    try recursive <|> return [ leftArgument ] 

end :: Parser ()
end = do
    whitespaceOrComment  
    string "end"
    whitespaceOrComment 

skipParser :: Parser Program
skipParser = do
    string "skip"
    whitespaceOrComment 
    return Skip

sendParser :: Parser Program
sendParser = do
            try $ string "{send "
            id1 <- identifierParser
            whitespaceOrComment 
            value <- identifierParser 
            whitespaceOrComment 
            char '}'
            whitespaceOrComment 
            return $ Send id1 value PID.master

threadHeader = do
    try $ string "thread"
    spaces
    optionMaybe threadWhereHeader

threadWhereHeader = do
    name <- try $ identifierParser <* string "where"
    spaces
    return name


threadParser :: Parser Program
threadParser = do
    typeName <- threadHeader
    whitespaceOrComment  
    work <- programParser
    end
    return $ SpawnThread typeName work

functionApplicationParser = do
    char '{'
    whitespaceOrComment 
    functionName <- identifierParser
    whitespaceOrComment 
    arguments <- many (identifierParser <* whitespaceOrComment )
    char '}'
    whitespaceOrComment 
    return $ Apply functionName arguments
    

 

identifierParser = do
    result <- Identifier <$> liftA2 (:) letter (many (letter <|> char '_'))
    whitespaceOrComment 
    return result


valueParser = 
    let
        trueParser = do
            string "true"
            whitespaceOrComment 
            return (VBool $ LiteralBool True)

        falseParser = do
            string "false"
            whitespaceOrComment 
            return (VBool $ LiteralBool False)

        receiveParser = do
            try $ string "{receive"
            whitespaceOrComment 
            name <- identifierParser
            char '}'
            whitespaceOrComment 
            return $ Receive name PID.master

        portParser = do
            string "port"
            return Port 

        intExpValue = 
            fmap (VInt . exprToIntExpr) intExprParser

        

    in
        choice 
            [ trueParser
            , falseParser
            , receiveParser
            , procedureParser
            , portParser
            , intExpValue
            ] 

procedureParser :: Parser Value
procedureParser = do
            try $ string "proc"
            whitespaceOrComment 
            char '{'
            whitespaceOrComment 
            arguments <- varList
            whitespaceOrComment 
            char '}'
            whitespaceOrComment 
            body <- programParser
            end
            return $ Procedure arguments body



varList = 
    let go = do
            spaces 
            identifierParser

    in
        liftA2 (:) identifierParser (many go)


boolExprParser :: Parser (Expr Bool)
boolExprParser = 
    let 
        atom :: Parser (Expr Bool)
        atom = do
            spaces
            result <- boolValueParser
            spaces
            return result
    in 
        choice 
            [ try $ boolOperator Equal "=="
            , try $ boolOperator LessThan "<"
            , try $ boolOperator LessThanEqual "<="
            , try $ boolOperator GreaterThan ">"
            , try $ boolOperator GreaterThanEqual ">="
            , atom
            ] 

intExprParser :: Parser (Expr Int)
intExprParser = 
    let 
        atom :: Parser (Expr Int)
        atom = do
            spaces
            result <- intValueParser
            spaces
            return result

    in
        choice 
            [ try $ operator Add '+'
            , try $ operator Subtract '-'
            , try $ operator Multiply '*'
            , try $ operator Divide '/'
            , atom 
            ] 



operator :: IntOperator -> Char -> Parser (Expr Int)
operator f opChar = do
    a <- intValueParser
    spaces
    char opChar
    spaces
    b <-  intExprParser
    spaces
    return $ EIntOperator f a b
            

intValueParser :: Parser (Expr Int)
intValueParser = do
    result <- fmap Reference identifierParser <|> fmap (Literal . read) (many1 digit)
    spaces
    return result



boolOperator :: BooleanOperator -> String -> Parser (Expr Bool)
boolOperator operatorValue operatorSymbol = do
    char '('
    spaces
    a <- intValueParser
    spaces
    string operatorSymbol
    spaces
    b <- intValueParser
    spaces
    char ')'
    spaces
    return $ EBoolOperator operatorValue a b
    
            
boolValueParser :: Parser (Expr Bool) 
boolValueParser = 
    let true = do
            string "true"
            spaces
            return $ Literal True
        false = do
            string "false"
            spaces
            return $ Literal False

    in
        choice
            [ true
            , false
            , fmap Reference identifierParser
            ] 

        
