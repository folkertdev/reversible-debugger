{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module DebuggerParser where 

import Control.Applicative ((<*), liftA2, pure)
import Data.List (intercalate)
import Data.Functor (($>))
import Data.Either
import Data.PID as PID (PID, create)
import qualified Data.Direction as Direction

import Text.ParserCombinators.Parsec as Parsec

import Data.Identifier as Identifier (Identifier, create)

import GHC.Generics
import Elm
import Data.Aeson (ToJSON, FromJSON)

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
        , sendReceiveNormalForm
        , help
        , quit 
        ]

data Result error a = Ok a | Err error deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

fromEither (Left e) = Err e
fromEither (Right v) = Ok v

toEither (Err e) = Left e 
toEither (Ok v) = Right v

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
    | Print (Result PID Identifier)
    | History (Result PID Identifier)
    | Help 
    | Quit
    | SendReceiveNormalForm Direction.Direction
    deriving (Eq, Show, Generic, ElmType, ToJSON, FromJSON)

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
    return $ Print $ fromEither id

history = do
    try (string "history") <|> string "h" 
    id <- threadOrIdentifier
    return $ History $ fromEither id

sendReceiveNormalForm :: Parser Instruction
sendReceiveNormalForm = do
    string "normalize" 
    return (SendReceiveNormalForm Direction.forward)


threadOrIdentifier :: Parser (Either PID Identifier) 
threadOrIdentifier = 
    ( Left <$> pid) <|> (Right <$> identifier)

identifier = do
    result <- Identifier.create <$> liftA2 (:) letter (many (letter <|> digit <|> char '_'))
    spaces 
    return result





pid = PID.create <$> int `sepBy` char '_'
    


int :: Parser Int
int = read <$> many1 digit
