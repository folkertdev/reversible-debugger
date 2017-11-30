module DebuggerParser where 

import Control.Applicative ((<*), liftA2, pure)
import Data.List (intercalate)
import Data.Functor (($>))
import Data.Either
import Data.PID as PID (PID, create)

import Text.ParserCombinators.Parsec as Parsec

import Types (Identifier(..))

parse :: String -> Either String Instruction
parse input = 
    case Parsec.parse parser "" input of
        Left _ -> Left $ "could not parse input: " ++ input 
        Right v -> Right v


parser :: Parser Instruction
parser = 
    choice $ map (\p -> try (p <* spaces <* eof))
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
        , skipLets
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
    | SkipLets
    deriving (Eq, Show)
        
forth :: Parser Instruction
forth = do
    try $ char 'f'
    optional $ string "orth"
    spaces
    id <- pid
    return $ Forth id

back = do
    try (string "back") <|> string "b"
    spaces
    id <- pid
    return $ Back id

roll = do
    try (string "roll") <|> string "r" 
    spaces
    id <- pid
    spaces
    n <- int
    spaces
    return $ Roll id n 

rollSend  = do
    try (string "rollsend") <|> string "rs" 
    spaces
    id <- identifier
    spaces
    n <- int
    spaces
    return $ RollSend id n

rollReceive = do
    try (string "rollreceive") <|> string "rr"
    spaces
    id <- identifier
    spaces
    n <- int
    spaces
    return $ RollReceive id n

rollThread = do
    try (string "rollthread") <|> string "rt"
    spaces
    id <- pid
    spaces
    return $ RollThread id  


rollVariable = do
    try (string "rollvariable") <|> string "rv"
    spaces
    id <- identifier
    spaces
    return $ RollVariable id  

run = do
    string "run" <|> string "r"
    return Run 

list = ListThreads <$ (try (string "list") <|> string "l")
store = Store <$ (try (string "store") <|> string "s")
help = Help <$ (try (string "help") <|> string "c")
quit = Quit <$ (try (string "quit") <|> string "q")

print = do
    try (string "print") <|> string "p"
    id <- threadOrIdentifier
    return $ Print id

history = do
    try (string "history") <|> string "h" 
    id <- threadOrIdentifier
    return $ History id

skipLets :: Parser Instruction
skipLets = do
    string "skiplets" 
    return SkipLets 


threadOrIdentifier :: Parser (Either PID Identifier) 
threadOrIdentifier = 
    ( Left <$> pid) <|> (Right <$> identifier)

identifier = do
    result <- Identifier <$> liftA2 (:) letter (many (letter <|> digit <|> char '_'))
    spaces 
    return result





pid = PID.create <$> int `sepBy` char '_'
    


int :: Parser Int
int = read <$> many1 digit
