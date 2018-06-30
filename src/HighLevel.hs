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
import Data.Void (Void)

newtype HighLevelProgram a = HighLevelProgram (StateT (Participant, Int) (Free (ProgramF Value)) a)
    deriving (Functor, Applicative, Monad, MonadState (Participant, Int), MonadFree (ProgramF Value))

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

fixToFree :: Fix (ProgramF value) -> Free (ProgramF value) a
fixToFree fix = 
    case fix of 
        Fix x -> Free (fmap fixToFree x)

depth :: Free (ProgramF value) Int -> Int
depth = Control.Monad.Free.iter helper 

helper :: ProgramF value Int -> Int
helper = foldr const 0

compile :: Participant -> HighLevelProgram Void -> Program Value
compile participant (HighLevelProgram program) = do
    let result = runStateT program (participant, 0) 
    freeToFix result

withCompile :: Participant -> HighLevelProgram a -> HighLevelProgram (Program Value)
withCompile participant (HighLevelProgram program) = do
    (_, n) <- State.get
    let newProgram = State.runStateT program (participant, n) 
        addedVariables = depth $ fmap (snd . snd) newProgram

    State.modify (\(p, n) -> (p, n + addedVariables))
    return $ freeToFix newProgram



send :: Value -> HighLevelProgram ()
send value = do
    (participant, _) <- State.get
    liftF (Send participant value ())  

receive :: HighLevelProgram Value
receive = do 
    (participant, _) <- State.get
    variableName <- uniqueVariableName 
    liftF (Receive participant variableName ())
    return (VReference variableName)


create :: Value -> HighLevelProgram Value
create value = do
    variableName <- uniqueVariableName 
    (owner, _) <- State.get
    liftF (Let owner variableName value ()) 
    return (VReference variableName)


ifThenElse :: Value -> HighLevelProgram a -> HighLevelProgram a -> HighLevelProgram a
ifThenElse condition thenBranch_ elseBranch_ = do
    (participant, n) <- State.get
    thenBranch :: Program Value <- withCompile participant thenBranch_
    elseBranch :: Program Value <- withCompile participant elseBranch_
    let program = fixToFree $ Fix $ IfThenElse participant condition thenBranch elseBranch
    HighLevelProgram $ lift program 




function :: (Value -> HighLevelProgram a) -> HighLevelProgram Value 
function body_ = do
    variableName <- uniqueVariableName 
    argument <- uniqueVariableName 

    (participant, n) <- State.get
    body <- withCompile participant (body_ (VReference argument))
    HighLevelProgram $ liftF (Let participant variableName (VFunction argument body) ())
    return (VReference variableName)

function2 :: (Value -> Value -> HighLevelProgram a) -> HighLevelProgram Value 
function2 body_ = do
    variableName <- uniqueVariableName 
    nestedName <- uniqueVariableName 
    argument1 <- uniqueVariableName 
    argument2 <- uniqueVariableName 

    (participant, _) <- State.get
    body <- withCompile participant (body_ (VReference argument1) (VReference argument2))
    let v1 = Let participant nestedName (VFunction argument2 body) (Fix NoOp)
        v2 = Let participant variableName (VFunction argument2 (Fix v1)) ()
            
    HighLevelProgram $ liftF v2 
    return (VReference variableName)

function3 :: (Value -> Value -> Value -> HighLevelProgram a) -> HighLevelProgram Value 
function3 body_ = do
    variableName <- uniqueVariableName 
    nested1Name <- uniqueVariableName 
    nested2Name <- uniqueVariableName 
    argument1 <- uniqueVariableName 
    argument2 <- uniqueVariableName 
    argument3 <- uniqueVariableName 

    (participant, _) <- State.get
    body <- withCompile participant (body_ (VReference argument1) (VReference argument2) (VReference argument3))
    let v1 = Let participant nested1Name (VFunction argument2 body) (Fix NoOp)
        v2 = Let participant nested2Name (VFunction argument2 (Fix v1)) (Fix NoOp)
        v3 = Let participant variableName (VFunction argument2 (Fix v2)) ()
            
    HighLevelProgram $ liftF v3
    return (VReference variableName)


recursiveFunction :: (Value -> Value -> HighLevelProgram a) -> HighLevelProgram Value 
recursiveFunction body_ = do
    variableName <- uniqueVariableName 
    argument <- uniqueVariableName 

    (participant, n) <- State.get
    body <- withCompile participant (body_ (VReference variableName) (VReference argument))
    HighLevelProgram $ liftF (Let participant variableName (VFunction argument body) ())
    return (VReference variableName)


recursive :: (HighLevelProgram a -> HighLevelProgram a) -> HighLevelProgram a
recursive body = do
    thunk <- recursiveFunction $ \self _ ->
        body (applyFunction self VUnit)

    applyFunction thunk VUnit


terminate :: HighLevelProgram a
terminate = HighLevelProgram (liftF NoOp)


offer :: List (String, HighLevelProgram a) -> HighLevelProgram a
offer options_ = do
    (participant, n) <- State.get
    let helper (label, program) = do
            newProgram <- withCompile participant program
            return ( label, newProgram )

    options :: List (String, Program Value) <- mapM helper options_
    HighLevelProgram $ lift (fixToFree $ Fix $ Offer participant options)

select :: List (String, Value, HighLevelProgram a) -> HighLevelProgram a
select options_ = do
    (participant, n) <- State.get
    let helper (label, condition, program) = do
            newProgram <- withCompile participant program
            return ( label, condition, newProgram )

    options <- mapM helper options_
    HighLevelProgram $ lift $ fixToFree (Fix $ Select participant options)

pick :: Label -> HighLevelProgram () -> HighLevelProgram ()
pick label program = select [ (label, VBool True, program) ]

option :: Label -> HighLevelProgram () -> (Label, HighLevelProgram ())
option = (,) 

selection :: Label -> Value -> HighLevelProgram () -> (Label, Value, HighLevelProgram ())
selection = (,,) 

inParallel :: List (Participant, HighLevelProgram a) -> HighLevelProgram () 
inParallel highlevelPrograms = do
    programs <- mapM (\(participant, hProgram) -> withCompile participant hProgram) highlevelPrograms 

    case programs of 
        [] -> 
            HighLevel.terminate

        (first:rest) -> 
            let program = List.foldl (\elem accum -> Fix $ Program.Parallel elem accum) first rest
            in HighLevelProgram $ lift $ fixToFree program
    

applyFunction :: Value -> Value -> HighLevelProgram a
applyFunction f argument = do
    (participant, _) <- State.get
    HighLevelProgram $ lift $ Free $ Application participant f argument

uniqueVariableName :: HighLevelProgram String
uniqueVariableName = do
    (participant, n) <- State.get
    State.put (participant, n + 1)
    return $ "var" ++ show n

variableCount :: HighLevelProgram Int 
variableCount = do
    (_, n) <- State.get
    return n

comparison eq a = 
    VComparison a eq

greaterThan = comparison GT 

lessThan = comparison LT 

equal = comparison EQ 
        -- deal or no deal? B can decide and retry when no deal
        -- otherwise finalize the deal
