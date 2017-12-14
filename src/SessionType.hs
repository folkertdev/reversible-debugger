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

import GHC.Generics
import Elm
import Data.Aeson


data LocalTypeState t = LocalTypeState { participant :: Identifier, state :: ExhaustableZipper (LocalAtom t) } deriving (Eq, Generic, ElmType, ToJSON, FromJSON)

instance Show t => Show (LocalTypeState t) where
    show LocalTypeState{participant, state} = "LocalTypeState " ++ unIdentifier participant ++ " " ++ show state 

deriving instance (ElmType a, ElmType b, ElmType c) => ElmType (a,b,c) 
data ExhaustableZipper a = Empty | Zipper (List a, a, List a) | Exhausted (List a) deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

isEmpty typeState = 
    case typeState of
        Empty -> True
        _ -> False

empty :: LocalTypeState t 
empty = LocalTypeState { participant = Identifier "unnamed session type", state = Empty }

fromLocalType :: Identifier -> LocalType t -> LocalTypeState t
fromLocalType participant localType =
    case localType of
        [] -> 
            LocalTypeState { participant = participant, state = Empty } 

        (x:xs) -> 
            LocalTypeState { participant = participant, state = Zipper ([], x, xs) } 

data SessionError t = ExhaustedForward | ExhaustedBackward | InvalidAction -- { expected :: t,  actual :: t}
    deriving (Eq, Show)

forwardWithSend :: LocalTypeState t -> Either (SessionError (LocalAtom t)) (Identifier, t, LocalTypeState t)
forwardWithSend typeState@LocalTypeState{state} = 
    case state of
        Empty -> 
            Except.throwError ExhaustedForward 

        Exhausted _ -> 
            Except.throwError ExhaustedForward 

        Zipper (previous, current, remaining) -> 
            case current of
                Send { receiver, type_ } -> 
                    case remaining of 
                        [] -> 
                            pure (receiver, type_, typeState { state = Exhausted (current:previous) }) 

                        (n:next) ->
                            pure (receiver, type_, typeState { state = Zipper (current:previous, n, next) })

                Receive { } -> 
                    Except.throwError InvalidAction

forwardWithReceive :: LocalTypeState t -> Either (SessionError (LocalAtom t)) (Identifier, t, LocalTypeState t)
forwardWithReceive typeState@LocalTypeState{state} = 
    case state of
        Empty -> 
            Except.throwError ExhaustedForward 

        Exhausted _ ->
            Except.throwError ExhaustedForward 

        Zipper (previous, current, remaining) -> 
            case current of
                Send {} -> 
                    Except.throwError InvalidAction

                Receive { sender, type_ } -> 
                    case remaining of
                        [] -> 
                            pure (sender, type_, typeState { state = Exhausted (current:previous) }) 

                        (n:next) ->
                            pure (sender, type_, typeState { state = Zipper (current:previous, n, next) })

backwardWithReceive :: LocalTypeState t -> Either (SessionError (LocalAtom t)) (Identifier, t, LocalTypeState t)
backwardWithReceive typeState =
    let 
        helper previous before next = 
            case previous of
                Send {} -> 
                    Except.throwError InvalidAction

                Receive { sender, type_ } -> 
                    pure (sender, type_, typeState { state = Zipper (before, previous, next) })
    in
        backwardWith helper typeState 

backwardWithSend :: LocalTypeState t -> Either (SessionError (LocalAtom t)) (Identifier, LocalTypeState t)
backwardWithSend typeState =
    let 
        helper previous before next = 
            case previous of
                Send {receiver} -> 
                    pure (receiver, typeState { state = Zipper (before, previous, next) })

                Receive {  } -> 
                    Except.throwError InvalidAction
    in
        backwardWith helper typeState


type Helper t1 b = LocalAtom t1 -> List (LocalAtom t1) -> List (LocalAtom t1) -> Either (SessionError (LocalAtom t1)) b 

backwardWith :: Helper t b -> LocalTypeState t -> Either (SessionError (LocalAtom t)) b 
backwardWith helper typeState@LocalTypeState{state} = 
    case state of
        Empty -> 
            Except.throwError ExhaustedBackward 

        Zipper ([], current, next) -> 
            Except.throwError ExhaustedBackward

        Exhausted [] -> 
            Except.throwError ExhaustedBackward

        Exhausted (previous:before) ->
            helper previous before []

        Zipper (previous:before, current, next) -> 
            helper previous before (current:next)



previousAction :: LocalTypeState t -> Maybe (LocalAtom t)
previousAction typeState@LocalTypeState{state} = 
    case state of
        Empty -> Nothing 
        Zipper ([], _, _) -> Nothing
        Zipper (h:hs, _, _) -> Just h
        Exhausted [] -> Nothing 
        Exhausted (h:hs) -> Just h


send :: Identifier -> t -> LocalAtom t 
send = Send

receive :: Identifier -> t -> LocalAtom t 
receive = Receive



data LocalAtom t = Send { receiver :: Identifier, type_ :: t } | Receive { sender :: Identifier, type_ :: t  } deriving (Eq, Generic, ElmType, ToJSON, FromJSON)

unIdentifier (Identifier x) = x


instance Show t => Show (LocalAtom t) where
    show Send { receiver, type_ } = unIdentifier receiver ++ "!" ++ ": <" ++ show type_ ++ "> "
    show Receive { sender, type_ } = unIdentifier sender ++ "?" ++ ": <" ++ show type_ ++ "> "


type LocalType t = List (LocalAtom t)

data GlobalAtom = Transaction { sender :: Identifier, receiver :: Identifier, tipe :: String } deriving (Show, Eq, Generic, ElmType, ToJSON, FromJSON)

prettyPrintGlobalAtom :: GlobalAtom -> String
prettyPrintGlobalAtom Transaction{ sender , receiver, tipe } = 
    show sender ++ " -> " ++ show receiver ++ " : <" ++ tipe ++ ">."

data GlobalType = GlobalType { parameters :: List Identifier, atoms :: List GlobalAtom } deriving (Eq, Show, Generic, ElmType, ToJSON, FromJSON)

instanciateGlobalType :: List Identifier -> GlobalType -> List GlobalAtom
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

deriveLocals :: List GlobalAtom -> Map Identifier (LocalType String)
deriveLocals atoms = Prelude.foldr go Map.empty atoms

go :: GlobalAtom -> Map Identifier (LocalType String) -> Map Identifier (LocalType String)
go atom@Transaction{ sender, receiver, tipe } = 
    let 
        inserter = Map.insertWith (++)
    in
        inserter sender [ Send { receiver = receiver, type_ = tipe } ] 
            . inserter receiver [ Receive { sender = sender, type_ = tipe } ]


data ParserGlobalTypeAtom = Action { sender :: Identifier, receiver :: Identifier, tipe :: String, continuation :: ParserGlobalTypeAtom} | End

identifier :: Parser Identifier
identifier = do
    result <- liftA2 (:) letter (many (letter <|> char '_'))
    spaces 
    return $ Identifier result

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
        try (fmap (: []) identifier) <|> multiple


parseGlobalType :: List Identifier -> String -> Either ParseError GlobalType
parseGlobalType parameters = 
    Parsec.parse (globalType parameters) "" 

globalType :: List Identifier -> Parser GlobalType
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

