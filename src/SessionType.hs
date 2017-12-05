{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}
module SessionType where 

import Text.ParserCombinators.Parsec as Parsec
import Control.Applicative (liftA2, (<*))
import Data.Traversable (for)

import Data.Map as Map

type List = []
type Identifier = String 

type MicroOzType = String

data LocalAtom = Send { receiver :: Identifier, tipe :: MicroOzType } | Receive { sender :: Identifier, tipe :: MicroOzType  } deriving (Eq)

instance Show LocalAtom where
    show (Send { receiver, tipe }) = receiver ++ "!" ++ " < " ++ tipe ++ " > "
    show (Receive { sender, tipe }) = sender ++ "?" ++ " < " ++ tipe ++ " > "




type LocalType = List LocalAtom

data GlobalAtom = Transaction { sender :: Identifier, receiver :: Identifier, tipe :: MicroOzType } deriving (Show, Eq)

prettyPrintGlobalAtom :: GlobalAtom -> String
prettyPrintGlobalAtom Transaction{ sender, receiver, tipe } = 
    sender ++ " -> " ++ receiver ++ " : <" ++ tipe ++ ">."

type GlobalType = List GlobalAtom

deriveLocals :: GlobalType -> Map Identifier LocalType
deriveLocals = Prelude.foldr go Map.empty

go :: GlobalAtom -> Map Identifier LocalType -> Map Identifier LocalType
go atom@Transaction{ sender, receiver, tipe } = 
    let 
        inserter = Map.insertWith (++)
    in
        inserter sender [ Send { receiver = receiver, tipe = tipe } ] 
            . inserter receiver [ Receive { sender = sender, tipe = tipe } ]

      
threeBuyerProtocol = 
    [ Transaction "A" "V" "title"
    , Transaction "V" "A" "price"
    , Transaction "V" "B" "price"
    , Transaction "A" "B" "share"
    , Transaction "B""A" "OK"
    , Transaction "B""V" "OK"
    , Transaction "B""C" "share"
    , Transaction "B""C" "thunk"
    , Transaction "B""V" "address"
    , Transaction "V""B" "date"
    ]

data ParserGlobalTypeAtom = Action { sender :: Identifier, receiver :: Identifier, tipe :: MicroOzType, continuation :: ParserGlobalTypeAtom} | End

identifier :: Parser Identifier
identifier = do
    result <- liftA2 (:) letter (many (letter <|> char '_'))
    spaces 
    return result

identifiers :: Parser (List Identifier)
identifiers = 
    let multiple = do
            string "{"
            spaces
            names <- sepBy1 identifier (string "," <* spaces) 
            string "}"
            spaces
            return names
    in
        try (fmap (\x -> [x]) identifier) <|> multiple


parseGlobalType :: String -> Either ParseError GlobalType
parseGlobalType rawString = 
    -- withoutWhitespace <- Parsec.parse eatComments "" rawString
    Parsec.parse globalType "" rawString

globalType :: Parser GlobalType
globalType = 
    let end = do 
            string "end"
            spaces 
    in
        Prelude.concat <$> manyTill transaction (try end) 


transaction :: Parser (List GlobalAtom) 
transaction = do
    sender <- identifier
    string "->"
    spaces
    receivers <- identifiers
    string ":"
    spaces
    annotation <- typeAnnotation
    string "."
    spaces
    return $ Prelude.map (\receiver -> Transaction{ sender = sender, receiver = receiver, tipe = annotation }) receivers
    
typeAnnotation :: Parser String
typeAnnotation = do
    string "<"
    spaces
    name <- manyTill anyChar (try (char '>'))
    spaces
    return name
    
example = 
    unlines 
        [ "A -> V: <title>.V -> {A, B} : <price>.A -> B : <share>."
            , "B -> {A, V} : <OK>."
            , "B -> C: <share>. B -> C : <thunk>."
            , "B -> V : <address>. V -> B : <date>. end"
        ]

