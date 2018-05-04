{-# LANGUAGE DeriveFunctor #-}
module Zipper where 

import Utils (List)

newtype Zipper a = Zipper (List a, a, List a)
    deriving (Eq, Show, Functor)

toList :: Zipper a -> List a 
toList (Zipper (a,b,c)) = a ++ (b:c)

fromSegments :: List a -> a -> List a -> Zipper a
fromSegments a b c = Zipper (a,b,c)

current :: Zipper a -> a
current (Zipper (_, v, _)) = v

map :: (a -> b) -> Zipper a -> Zipper b
map = fmap
