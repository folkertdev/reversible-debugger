module Data.PID (PID, create, parent, child, master) where 

import qualified Data.List as List

newtype PID = PID [Int] deriving (Eq, Ord) 

instance Show PID where
    show (PID list) = 
        List.intercalate "_" $ map show list

create :: [Int] -> PID
create = PID

parent :: PID -> PID
parent (PID list) = PID (List.init list)

child :: Int -> PID -> PID 
child n (PID list) = PID (list ++ [n])

master :: PID
master = PID [0]
