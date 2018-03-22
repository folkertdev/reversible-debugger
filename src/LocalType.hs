{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, PatternSynonyms, DuplicateRecordFields #-}
module LocalType where 

import GHC.Generics

import Types ((|>))
import Data.Map as Map 
import Data.Set as Set 
import Data.Fix
import Data.Map.Merge.Strict as Merge

import GlobalType (GlobalType)
import qualified GlobalType

type Participant = String
type Identifier = String
type Location = String

{-| A local type with send, receive, recursion, choice and offer -}
type LocalType u = Fix (LocalTypeF u)

data LocalTypeF u f 
    = Transaction (Transaction u f)
    | Choice f f 
    | Offer f f
    | Atom (Atom f)
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Atom f = R f | V | Wk f | End
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Transaction u f 
    = TSend { owner :: Participant, receiver :: Participant, tipe :: u, continuation :: f } 
    | TReceive { owner :: Participant, sender :: Participant, variableName :: Maybe Identifier, tipe ::  u, continuation :: f } 
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


-- PATTERNS

pattern BackwardSend owner participant tipe continuation = 
    SendOrReceive (Transaction (TSend owner participant tipe ())) continuation

pattern BackwardReceive owner participant visibleName tipe continuation = 
    SendOrReceive (Transaction (TReceive owner participant (Just visibleName) tipe ())) continuation

pattern Send owner participant tipe continuation = 
    Transaction (TSend owner participant tipe continuation)

pattern Receive owner participant tipe continuation = 
    Transaction (TReceive owner participant Nothing tipe continuation)

pattern RecursionPoint rest = Atom (R rest)
pattern WeakenRecursion rest = Atom (Wk rest)
pattern RecursionVariable = Atom V


{-| Data structure containing the information needed to roll an action -}
type TypeContext program value a = Fix (TypeContextF program value a)

data TypeContextF program value a f 
    = Hole 
    | SendOrReceive (LocalTypeF a ()) f 
    | Branched { condition :: value, verdict :: Bool, otherBranch :: (program, LocalType a), continuation :: f }
    | Application Identifier f 
    | Spawning Location Location Location f
    | Assignment { visibleName :: Identifier, internalName :: Identifier, continuation :: f }
    | Literal a f
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


{-| The state of a local type is 
 * the information needed to roll, the TypeContext
 * the information needed to step forward, the LocalType
-}
type LocalTypeState program value u =  ( TypeContext program value u, LocalType u ) 

recurse :: LocalType a -> LocalType a 
recurse cont = Fix . Atom $ R cont

broadenScope :: LocalType a -> LocalType a 
broadenScope cont = Fix . Atom $ Wk cont

recursionVariable :: LocalType a
recursionVariable = Fix $ Atom V

end :: LocalType a
end = Fix $ Atom End

send :: Participant ->  Participant -> u -> LocalType u -> LocalType u 
send owner sender tipe = Fix . Transaction . TSend owner sender tipe

receive :: Participant -> Participant -> u -> LocalType u -> LocalType u 
receive owner receiver tipe = Fix . Transaction . TReceive owner receiver Nothing tipe


projections :: GlobalType u -> Map Participant (LocalType u)
projections global = 
    GlobalType.participants global
        |> Set.foldr (\participant -> Map.insert participant (project participant global)) Map.empty 


{-| Project a global type into a local one for a particular participant -}
project :: Participant -> GlobalType u -> LocalType u 
project participant = 
    Data.Fix.cata $ \global -> 
        case global of 
            GlobalType.Transaction sender receiver tipe cont -> 
                if participant == sender then 
                     LocalType.send sender receiver tipe cont
                else if participant == receiver then 
                     LocalType.receive receiver sender tipe cont
                else 
                    cont 

            GlobalType.R nestedGlobalType -> 
                recurse nestedGlobalType 

            GlobalType.V -> 
                recursionVariable

            GlobalType.Wk nestedGlobalType -> 
                broadenScope nestedGlobalType

            GlobalType.End -> 
                end

