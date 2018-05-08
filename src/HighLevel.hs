{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HighLevel where 

import Control.Monad.State as State
import LocalType (Location, Identifier, Participant)
import Program (ProgramF(..), Value(..), Program, terminate)
import qualified Semantics
import Control.Monad.Free
import Data.Fix
import Utils (List)
import Data.List as List
import Control.Arrow (second)
import Control.Applicative
import Data.Foldable (foldr1)
import Debug.Trace as Debug

newtype HighLevelProgram a = HighLevelProgram (StateT (Location, Participant, Int) (Free (ProgramF Value)) a)
    deriving (Functor, Applicative, Monad, MonadState (Location, Participant, Int))

type Label = String

{-
instance Alternative HighLevelProgram where 
    empty = terminate 

    a <|> b = do
        (location, participant, originalN) <- State.get
        program1 <- withCompile location participant originalN a
        case unFix program1 of 
            NoOp -> do
                State.put (location, participant, originalN) 
                b

            _ -> do
                State.put (location, participant, originalN) 
                a 
-}



freeToFix :: Free (ProgramF value) a -> Program value 
freeToFix free =
    case free of 
        Pure n -> Program.terminate
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

compile :: Location -> Participant -> HighLevelProgram a -> Program Value
compile location participant (HighLevelProgram program) = do
    let result = runStateT program (location, participant, 0) 
    freeToFix result

withCompile :: Location -> Participant -> Int -> HighLevelProgram a -> HighLevelProgram (Program Value)
withCompile location participant n (HighLevelProgram program) = do
    let newProgram = State.runStateT program (location, participant, n) 
        addedVariables = Debug.traceShowId $ depth $ fmap ((\(_,_,n) -> n) . snd) newProgram

    State.modify (\(l, p, n) -> (l, p, n + addedVariables))
    return $ freeToFix newProgram

myProgram :: HighLevelProgram ()
myProgram = do
    send (VInt 4)
    x <- receive
    send x
    return ()



liftFree :: Functor f => f a -> Free f a
liftFree action = Free (fmap Pure action)

send :: Value -> HighLevelProgram ()
send value = do
    (_, participant, _) <- State.get
    HighLevelProgram $ lift $ liftFree (Send participant value ())  

receive :: HighLevelProgram Value
receive = do 
    (_, participant, _) <- State.get
    variableName <- uniqueVariableName 
    HighLevelProgram $ lift $ liftFree (Receive participant variableName ())
    return (VReference variableName)


create :: Value -> HighLevelProgram Value
create value = do
    variableName <- uniqueVariableName 
    HighLevelProgram $ lift $ liftFree (Let variableName value ()) 
    return (VReference variableName)


ifThenElse :: Value -> HighLevelProgram a -> HighLevelProgram b -> HighLevelProgram ()
ifThenElse condition thenBranch_ elseBranch_ = do
    (location, participant, n) <- State.get
    thenBranch :: Program Value <- withCompile location participant n thenBranch_
    elseBranch :: Program Value <- withCompile location participant n elseBranch_
    let program = fixToFree $ Fix $ IfThenElse condition thenBranch elseBranch
    HighLevelProgram $ lift program 




function :: (Value -> HighLevelProgram a) -> HighLevelProgram Value 
function body_ = do
    variableName <- uniqueVariableName 
    argument <- uniqueVariableName 

    (location, participant, n) <- State.get
    body <- withCompile location participant n (body_ (VReference argument))
    HighLevelProgram $ lift $ liftFree (Let variableName (VFunction argument body) ())
    return (VReference variableName)

recursiveFunction :: (Value -> Value -> HighLevelProgram a) -> HighLevelProgram Value 
recursiveFunction body_ = do
    variableName <- uniqueVariableName 
    argument <- uniqueVariableName 

    (location, participant, n) <- State.get
    body <- withCompile location participant n (body_ (VReference variableName) (VReference argument))
    HighLevelProgram $ lift $ liftFree (Let variableName (VFunction argument body) ())
    return (VReference variableName)

terminate :: HighLevelProgram a
terminate = HighLevelProgram (lift $ Free NoOp)


offer :: List (String, HighLevelProgram ()) -> HighLevelProgram ()
offer options_ = do
    (location, participant, n) <- State.get
    let helper (label, program) = do
            newProgram <- withCompile location participant n program
            return ( label, newProgram )

    options <- mapM helper options_

    HighLevelProgram $ lift $ fixToFree (Fix $ Offer participant options)

select :: List (String, Value, HighLevelProgram ()) -> HighLevelProgram ()
select options_ = do
    (location, participant, n) <- State.get
    let helper (label, condition, program) = do
            newProgram <- withCompile location participant n program
            return ( label, condition, newProgram )

    options <- mapM helper options_
    HighLevelProgram $ lift $ fixToFree (Fix $ Select participant options)

pick :: Label -> HighLevelProgram () -> HighLevelProgram ()
pick label program = select [ (label, VBool True, program) ]

option :: Label -> HighLevelProgram () -> (Label, HighLevelProgram ())
option = (,) 

selection :: Label -> Value -> HighLevelProgram () -> (Label, Value, HighLevelProgram ())
selection = (,,) 
    

applyFunction :: Value -> Value -> HighLevelProgram ()
applyFunction (VReference f) argument = HighLevelProgram $ lift $ Free $ Application f argument
applyFunction _ _ = error "functions atm. can only be references" 

uniqueVariableName :: HighLevelProgram String
uniqueVariableName = do
    (location, participant, n) <- State.get
    State.put (location, participant, n + 1)
    return $ "var" ++ show n

variableCount :: HighLevelProgram Int 
variableCount = do
    (_, _, n) <- State.get
    return n

comparison eq a = 
    VComparison a eq

greaterThan = comparison GT 

lessThan = comparison LT 

equal = comparison EQ 
