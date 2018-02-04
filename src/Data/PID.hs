{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.PID (PID, create, parent, child, master, nonsense) where 

import qualified Data.List as List

import GHC.Generics
import Elm
import Data.Proxy
import Data.Aeson

newtype PID = PID [Int] deriving (Eq, Ord, Generic, ElmType, ToJSON, ToJSONKey, FromJSON, FromJSONKey) 

instance Show PID where
    show (PID list) = 
        List.intercalate "_" $ map show list

instance HasElmComparable PID where
    toElmComparable _ = EList (toElmType (Proxy :: Proxy Int))

create :: [Int] -> PID
create = PID

parent :: PID -> PID
parent (PID list) = PID (List.init list)

child :: Int -> PID -> PID 
child n (PID list) = PID (list ++ [n])

master :: PID
master = PID [0]

nonsense :: PID
nonsense = PID [-42]
