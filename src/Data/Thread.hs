module Data.Thread where 

import Types


{-| An individual thread, with 

* a unique identifier
* list of history instructions 
* list of remaining program instructions
-}
data Thread history a = Thread PID (List history) (List a) deriving (Show, Eq)

pid :: Thread history a -> PID 
pid (Thread pid_ _ _) = pid_


history :: Thread history a -> List history 
history (Thread _ history_ _) = history_

program :: Thread history a -> List a
program (Thread _ _ program_) = program_
