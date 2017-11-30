module MicroOz.Parser (program) where

import Control.Applicative ((<*), liftA2, pure)
import Data.List (intercalate)

import Text.ParserCombinators.Parsec as Parsec

import Types (Identifier(..))
import MicroOz
import Queue

program :: String -> Either ParseError Program
program rawString = do 
    withoutWhitespace <- Parsec.parse eatComments "" rawString
    Parsec.parse programParser "" withoutWhitespace



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
    return $ If condition trueBody falseBody

assertParser = do
    try $ string "assert"
    whitespaceOrComment
    condition <- boolExprParser
    whitespaceOrComment
    return $ Assert condition


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
            return $ Send id1 value

threadParser :: Parser Program
threadParser = do
            try $ string "thread"
            whitespaceOrComment  
            work <- programParser
            end
            return $ SpawnThread work

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
            return (VBool $ Literal True)

        falseParser = do
            string "false"
            whitespaceOrComment 
            return (VBool $ Literal False)

        receiveParser = do
            try $ string "{receive"
            whitespaceOrComment 
            name <- identifierParser
            char '}'
            whitespaceOrComment 
            return $ Receive name

        portParser = do
            string "port"
            return Port 

        intExpValue = 
            fmap VInt intExprParser

        

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
    return $ IntOperator f a b
            

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
    return $ BoolOperator operatorValue a b
    
            
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

        
