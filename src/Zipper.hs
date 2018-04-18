module Zipper where 

import Types (List)

newtype Zipper a = Zipper (List a, a, List a)
    deriving (Eq, Show)

toList :: Zipper a -> List a 
toList (Zipper (a,b,c)) = a ++ (b:c)

fromSegments :: List a -> a -> List a -> Zipper a
fromSegments a b c = Zipper (a,b,c)
