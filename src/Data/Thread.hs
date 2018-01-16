{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Thread where 

import Types
import qualified SessionType 
import Data.PID as PID (PID)
import Data.Actor (Actor)

import GHC.Generics
import Elm
import Data.Aeson

{-| An individual thread, with 

* a unique identifier
* list of history instructions 
* list of remaining program instructions
-}
data Thread history a 
    = Thread 
            { pid :: PID
            , actor :: Actor
            , history :: List history
            , program :: List a 
            } deriving (Eq, Generic, ElmType, ToJSON, FromJSON)

instance (Show history, Show a) => Show (Thread history a) where 
    show (Thread pid actor history program) = 
        "Thread " ++ show pid ++ " - " ++ show actor 
            ++ "\n"
            ++ "\n"
            ++ "instruction stack: "
            ++ show program
            ++ "\n"
            ++ "\n"
            ++ "history stack: "
            ++ show history
