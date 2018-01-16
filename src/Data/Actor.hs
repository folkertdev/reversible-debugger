{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Actor (Participant, Actor, named, unnamed, toList, push, pop) where 

import Types

import GHC.Generics
import Elm
import Data.Aeson

type Participant = Identifier

data Actor = Actor [Participant] 
    deriving (Eq, Generic, ElmType, ToJSON, FromJSON, Show)

unnamed = Actor []

named x = Actor [x]

push x (Actor xs) = Actor (x:xs)

pop (Actor names) = 
    case names of 
        [] -> unnamed
        (x:xs) -> Actor xs


toList :: Actor -> List Participant
toList (Actor xs) = xs

instance Monoid Actor where
    mempty = Actor []
    mappend (Actor xs) (Actor ys) = Actor (xs ++ ys)
