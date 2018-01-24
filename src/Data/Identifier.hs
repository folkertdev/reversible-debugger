{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Identifier where 

import GHC.Generics
import Elm
import Data.Proxy
import Data.Aeson

newtype Identifier = Identifier { unwrap :: String } 
    deriving (Show, Eq, Ord, Generic, ElmType, ToJSON, ToJSONKey, FromJSON, FromJSONKey) 


instance HasElmComparable Identifier where 
    toElmComparable = toElmComparable . unwrap

create :: String -> Identifier
create = Identifier



newtype PID = PID [Int] 
type ChannelName = Identifier
