module Data.Thread where 

import Types
import qualified SessionType 
import Data.PID as PID (PID)


{-| An individual thread, with 

* a unique identifier
* list of history instructions 
* list of remaining program instructions
-}
data Thread history a 
    = Thread 
            { pid :: PID
            , typeState :: SessionType.LocalTypeState String
            , history :: List history
            , program :: List a 
            } deriving (Eq)

instance (Show history, Show a) => Show (Thread history a) where 
    show (Thread pid localType history program) = 
        "Thread " ++ show pid 
            ++ "\n"
            ++ "local type state"
            ++ show localType
            ++ "\n"
            ++ "\n"
            ++ "instruction stack: "
            ++ show program
            ++ "\n"
            ++ "\n"
            ++ "history stack: "
            ++ show history
