{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Actor (Participant, Actor, named, unnamed, toList, push, pop, participant, unParticipant, currentParticipant) where 

import Data.Identifier as Identifier (Identifier, unwrap, create)

import GHC.Generics
import Elm
import Data.Aeson


newtype Participant = Participant Identifier
    deriving (Eq, Ord, Generic, ElmType, ToJSON, FromJSON, Show, ToJSONKey, FromJSONKey)

unParticipant :: Participant -> String
unParticipant (Participant identifier) = Identifier.unwrap identifier

participant :: String -> Participant 
participant = Participant . Identifier.create

instance HasElmComparable Participant where
    toElmComparable (Participant i) = toElmComparable i


newtype Actor = Actor [Participant] 
    deriving (Eq, Generic, ElmType, ToJSON, FromJSON, Show)

unnamed = Actor []

named x = Actor [x]

push x (Actor xs) = Actor (x:xs)

pop (Actor names) = 
    case names of 
        [] -> unnamed
        (x:xs) -> Actor xs


toList :: Actor -> [Participant]
toList (Actor xs) = xs

currentParticipant :: Actor -> Maybe Participant
currentParticipant (Actor participants) = 
    case participants of 
        [] -> Nothing 
        x:xs -> Just x

instance Monoid Actor where
    mempty = Actor []
    mappend (Actor xs) (Actor ys) = Actor (xs ++ ys)
