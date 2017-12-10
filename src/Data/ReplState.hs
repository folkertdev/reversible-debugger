{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.ReplState where

import GHC.Generics
import Data.Proxy
import Elm
import Data.Aeson (FromJSON, ToJSON)


import qualified Data.Text as Text
import Data.Monoid ((<>))


import Types
import SessionType
import Queue
import MicroOz
import Data.Context
import Data.Thread
import Data.ThreadState
import Data.PID
import DebuggerParser


data ReplState = ReplState { context :: Context Value, threadState ::  ThreadState History Program } deriving (Show, Generic, ElmType, ToJSON, FromJSON)
