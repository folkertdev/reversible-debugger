{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Direction (Direction, forward, backward, directed) where

import GHC.Generics
import Elm
import Data.Proxy
import Data.Aeson

data Direction = Forward | Backward deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

forward = Forward
backward = Backward 

directed direction forForward forBackward = 
    case direction of 
        Forward -> 
            forForward

        Backward -> 
            forBackward
