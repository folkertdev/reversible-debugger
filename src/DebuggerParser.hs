module DebuggerParser where 

import Control.Applicative ((<*), liftA2, pure)
import Data.List (intercalate)
import Data.Functor (($>))
import Data.Either

import Text.ParserCombinators.Parsec as Parsec

import Types (Identifier(..), PID)

parse :: String -> Either String Instruction
parse input = 
    case Parsec.parse parser "" input of
        Left _ -> Left $ "could not parse input: " ++ input 
        Right v -> Right v


parser :: Parser Instruction
parser = 
    choice $ map try
        [ forth
        , back
        , roll
        , rollSend
        , rollReceive 
        , rollThread
        , rollVariable
        , run
        , list
        , store
        , DebuggerParser.print
        , history
        , help
        , quit 
        ]
            


data Instruction 
    = Forth PID
    | Back PID
    | Roll PID Int
    | RollSend Identifier Int
    | RollReceive Identifier Int 
    | RollThread PID
    | RollVariable Identifier
    | Run 
    | ListThreads 
    | Store
    | Print (Either PID Identifier)
    | History (Either PID Identifier)
    | Help 
    | Quit
        
forth :: Parser Instruction
forth = do
    string "f" <|> string "forth" 
    spaces
    id <- pid
    return $ Forth id

back = do
    string "b" <|> string "back" 
    spaces
    id <- pid
    return $ Back id

roll = do
    string "r" <|> string "roll" 
    spaces
    id <- pid
    spaces
    n <- int
    spaces
    return $ Roll id n 

rollSend  = do
    string "rs" <|> string "rollsend" 
    spaces
    id <- identifier
    spaces
    n <- int
    spaces
    return $ RollSend id n

rollReceive = do
    string "rr" <|> string "rollreceive" 
    spaces
    id <- identifier
    spaces
    n <- int
    spaces
    return $ RollReceive id n

rollThread = do
    string "rt" <|> string "rollthread" 
    spaces
    id <- pid
    spaces
    n <- int
    spaces
    return $ RollThread id  


rollVariable = do
    string "rv" <|> string "rollvariable" 
    spaces
    id <- identifier
    spaces
    n <- int
    spaces
    return $ RollVariable id  

run = do
    string "r" <|> string "run" 
    return $ Run 

list = ListThreads <$ (string "l" <|> string "list")
store = Store <$ (string "s" <|> string "store")
help = Help <$ (string "c" <|> string "help")
quit = Quit <$ (string "q" <|> string "quit")

print = do
    string "p" <|> string "print" 
    id <- threadOrIdentifier
    return $ Print id

history = do
    string "h" <|> string "history" 
    id <- threadOrIdentifier
    return $ History id



threadOrIdentifier :: Parser (Either PID Identifier) 
threadOrIdentifier = 
    ( Left <$> pid) <|> (Right <$> identifier)

identifier = do
    result <- Identifier <$> liftA2 (:) letter (many (letter <|> char '_'))
    spaces 
    return result





pid = int `sepBy` spaces 
    


int :: Parser Int
int = read <$> many1 digit
