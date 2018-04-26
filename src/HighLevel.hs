module HighLevel where 

import Control.Monad.State as State
import LocalType (Location, Identifier, Participant)
import Semantics (ProgramF(..), Value(..), Program)
import qualified Semantics
import Control.Monad.Free
import Data.Fix
import Utils (List)
import Data.List as List
import Control.Arrow (second)
import Data.Foldable (foldr1)
import Debug.Trace as Debug

type MyProgram a = StateT (Location, Participant, Int) (Free (ProgramF Value)) a

freeToFix :: Free (ProgramF value) a -> Program value 
freeToFix free =
    case free of 
        Pure n -> Semantics.terminate
        Free x -> Fix (fmap freeToFix x)

fixToFree :: Fix (ProgramF value) -> Free (ProgramF value) ()
fixToFree fix = 
    case fix of 
        Fix NoOp -> Pure ()
        Fix x -> Free (fmap fixToFree x)

depth :: Free (ProgramF value) Int -> Int
depth = Control.Monad.Free.iter helper 

helper :: ProgramF value Int -> Int
helper = foldr const 0

compile :: Location -> Participant -> MyProgram a -> Program Value
compile location participant program = do
    let result = runStateT program (location, participant, 0) 
    freeToFix result

withCompile :: Location -> Participant -> Int -> MyProgram a -> MyProgram (Program Value)
withCompile location participant n program = do
    let newProgram = State.runStateT program (location, participant, n) 
        addedVariables = Debug.traceShowId $ depth $ fmap ((\(_,_,n) -> n) . snd) newProgram

    State.modify (\(l, p, n) -> (l, p, n + addedVariables))
    return $ freeToFix newProgram

myProgram :: MyProgram ()
myProgram = do
    send (VInt 4)
    x <- receive
    send x
    return ()



liftFree :: Functor f => f a -> Free f a
liftFree action = Free (fmap Pure action)

send :: Value -> MyProgram ()
send value = do
    (_, participant, _) <- State.get
    lift $ liftFree (Send participant value ())  

receive :: MyProgram Value
receive = do 
    (_, participant, _) <- State.get
    variableName <- myProgramVariableName 
    lift $ liftFree (Receive participant variableName ())
    return (VReference variableName)

create :: Value -> MyProgram Value
create value = do
    variableName <- myProgramVariableName 
    lift $ liftFree (Let variableName value ()) 
    return (VReference variableName)


function :: (Value -> MyProgram a) -> MyProgram Value 
function body_ = do
    variableName <- myProgramVariableName 
    argument <- myProgramVariableName 

    (location, participant, n) <- State.get
    body <- withCompile location participant n (body_ (VReference argument))
    lift $ liftFree (Let variableName (VFunction argument body) ())
    return (VReference variableName)

recursiveFunction :: (Value -> Value -> MyProgram a) -> MyProgram Value 
recursiveFunction body_ = do
    variableName <- myProgramVariableName 
    argument <- myProgramVariableName 

    (location, participant, n) <- State.get
    body <- withCompile location participant n (body_ (VReference variableName) (VReference argument))
    lift $ liftFree (Let variableName (VFunction argument body) ())
    return (VReference variableName)

terminate :: MyProgram ()
terminate = return ()

offer :: List (String, MyProgram ()) -> MyProgram ()
offer options_ = do
    (location, participant, n) <- State.get
    let helper (label, program) = do
            newProgram <- withCompile location participant n program
            return ( label, newProgram )

    options <- mapM helper options_

    lift $ fixToFree (Fix $ Offer participant options)

select :: List (String, Value, MyProgram ()) -> MyProgram ()
select options_ = do
    (location, participant, n) <- State.get
    let helper (label, condition, program) = do
            newProgram <- withCompile location participant n program
            return ( label, condition, newProgram )

    options <- mapM helper options_
    lift $ fixToFree (Fix $ Select participant options)
    

applyFunction :: Value -> Value -> MyProgram ()
applyFunction (VReference f) argument = lift $ Free $ Application f argument
applyFunction _ _ = error "functions atm. can only be references" 

myProgramVariableName :: MyProgram String
myProgramVariableName = do
    (location, participant, n) <- State.get
    State.put (location, participant, n + 1)
    return $ "var" ++ show n

variableCount :: MyProgram Int 
variableCount = do
    (_, _, n) <- State.get
    return n

