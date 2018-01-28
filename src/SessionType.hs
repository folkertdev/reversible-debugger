{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, FlexibleContexts, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module SessionType where 

import Text.ParserCombinators.Parsec as Parsec
import Control.Applicative (liftA2, (<*))
import qualified Control.Monad.Except as Except
import Data.Traversable (for)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.String
import Types 
import Data.Actor as Participant (Participant, participant, unParticipant)

import GHC.Generics
import Elm
import Data.Aeson


data LocalAtom t = Send { receiver :: Participant, type_ :: t } | Receive { sender :: Participant, type_ :: t  } deriving (Eq, Generic, ElmType, ToJSON, FromJSON)


instance Show t => Show (LocalAtom t) where
    show Send { receiver, type_ } = Participant.unParticipant receiver ++ "!" ++ ": <" ++ show type_ ++ "> "
    show Receive { sender, type_ } = Participant.unParticipant sender ++ "?" ++ ": <" ++ show type_ ++ "> "


type LocalType t = List (LocalAtom t)
data LocalTypeState t = LocalTypeState { participant :: Participant, state :: ExhaustableZipper (LocalAtom t) } deriving (Eq, Generic, ElmType, ToJSON, FromJSON)

instance Show t => Show (LocalTypeState t) where
    show LocalTypeState{participant, state} = "LocalTypeState " ++ unParticipant participant ++ " " ++ show state 

deriving instance (ElmType a, ElmType b, ElmType c) => ElmType (a,b,c) 
data ExhaustableZipper a 
    = Empty 
    | Zipper (List a, a, List a) 
    | ExhaustedForward (List a) a
    | ExhaustedBackward a (List a)
    deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

isEmpty typeState = 
    case typeState of
        Empty -> True
        _ -> False

empty :: LocalTypeState t 
empty = LocalTypeState { participant = Participant.participant "unnamed session type", state = Empty }

fromLocalType :: Participant -> LocalType t -> LocalTypeState t
fromLocalType participant localType =
    case localType of
        [] -> 
            LocalTypeState { participant = participant, state = Empty } 

        (x:xs) -> 
            LocalTypeState { participant = participant, state = Zipper ([], x, xs) } 

data ReachedEnd = ReachedEnd deriving (Show, Eq)
data ReachedBegin = ReachedBegin deriving (Show, Eq) 

forward :: LocalTypeState t -> Either ReachedEnd (LocalAtom t, LocalTypeState t)
forward typeState@LocalTypeState{state} = 
    case state of
        Empty -> 
            Except.throwError ReachedEnd

        ExhaustedForward _ _ -> 
            Except.throwError ReachedEnd 

        ExhaustedBackward current rest -> 
            forward (typeState { state = Zipper ([], current, rest) })

        Zipper (previous, current, next:remaining) -> 
            pure (current, typeState { state = Zipper (current:previous, next, remaining)})

        Zipper (previous, current, []) -> 
            pure (current, typeState { state = ExhaustedForward previous current })


backward :: LocalTypeState t -> Either ReachedBegin (LocalAtom t, LocalTypeState t)
backward typeState@LocalTypeState{state} = 
    case state of
        Empty -> 
            Except.throwError ReachedBegin

        ExhaustedForward previous current -> 
            backward (typeState { state = Zipper (previous, current, []) } )

        ExhaustedBackward _ _ -> 
            Except.throwError ReachedBegin 

        Zipper (next:previous, current, remaining) -> 
            pure (current, typeState { state = Zipper (previous, next, current:remaining)})

        Zipper ([], current, remaining) -> 
            pure (current, typeState)


data GlobalAtom = Transaction { sender :: Participant, receiver :: Participant, tipe :: String } deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

prettyPrintGlobalAtom :: GlobalAtom -> String
prettyPrintGlobalAtom Transaction{ sender , receiver, tipe } = 
    show sender ++ " -> " ++ show receiver ++ " : <" ++ tipe ++ ">."

data GlobalType = GlobalType { parameters :: List Participant, atoms :: List GlobalAtom } deriving (Eq, Show, Generic, ElmType, ToJSON, FromJSON)

instanciateGlobalType :: List Participant -> GlobalType -> List GlobalAtom
instanciateGlobalType arguments GlobalType{ parameters, atoms } = 
    let 
        replace old new actual = 
            if actual == old then new else actual

        renameInTransaction old new Transaction{sender, receiver, tipe} = 
            Transaction { sender = replace old new sender, receiver = replace old new receiver, tipe = tipe } 
        
        tagger :: GlobalAtom -> GlobalAtom
        tagger = foldr (.) id $ zipWith renameInTransaction parameters arguments
    in
        map tagger atoms

deriveLocals :: List GlobalAtom -> Map Participant (LocalType String)
deriveLocals = Prelude.foldr go Map.empty 

go :: GlobalAtom -> Map Participant (LocalType String) -> Map Participant (LocalType String)
go atom@Transaction{ sender, receiver, tipe } = 
    let 
        inserter = Map.insertWith (++)
    in
        inserter sender [ Send { receiver = receiver, type_ = tipe } ] 
            . inserter receiver [ Receive { sender = sender, type_ = tipe } ]


data ParserGlobalTypeAtom = Action { sender :: Participant, receiver :: Participant, tipe :: String, continuation :: ParserGlobalTypeAtom} | End

identifier :: Parser Participant
identifier = do
    result <- liftA2 (:) letter (many (letter <|> char '_'))
    spaces 
    return $ Participant.participant result

identifiers :: Parser (List Participant)
identifiers = 
    let multiple = do
            string "{"
            spaces
            names <- sepBy1 identifier (string "," <* spaces) 
            string "}"
            spaces
            return names
    in
        try (fmap (: []) identifier) <|> multiple


parseGlobalType :: List Participant -> String -> Either ParseError GlobalType
parseGlobalType parameters = 
    Parsec.parse (globalType parameters) "" 

globalType :: List Participant -> Parser GlobalType
globalType parameters = 
    let end = do 
            string "end"
            spaces 
    in
        GlobalType parameters . Prelude.concat <$> manyTill transaction (try end) 


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

