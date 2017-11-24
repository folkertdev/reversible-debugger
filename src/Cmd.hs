module Cmd where 
import Control.Applicative
import Types


data Cmd a = Empty | Singleton a | Batched [a] 

none :: Cmd a
none = Empty

batch :: List (Cmd a) -> Cmd a 
batch commands = 
    Batched (concatMap unpack commands) 


unpack command = 
    case command of 
        Empty -> []
        Singleton x -> [x]
        Batched xs -> xs

map :: (a -> b) -> Cmd a -> Cmd b
map tagger command = 
    case command of 
        Empty -> Empty
        Singleton x -> Singleton (tagger x)
        Batched xs -> Batched (fmap tagger xs)

create = Singleton

instance Monoid (Cmd a) where 
    mempty = Empty

    mappend a b = batch [a,b]
