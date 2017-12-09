{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.Thread where 

import Types
import qualified SessionType 
import Data.PID as PID (PID)

import GHC.Generics
import Elm

{-| An individual thread, with 

* a unique identifier
* list of history instructions 
* list of remaining program instructions
-}
data Thread history a 
    = Thread 
            { pid :: PID
            , history :: List history
            , program :: List a 
            } deriving (Eq, Generic, ElmType)

instance (Show history, Show a) => Show (Thread history a) where 
    show (Thread pid history program) = 
        "Thread " ++ show pid 
            ++ "\n"
            ++ "\n"
            ++ "instruction stack: "
            ++ show program
            ++ "\n"
            ++ "\n"
            ++ "history stack: "
            ++ show history
