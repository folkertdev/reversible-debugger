{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, PatternSynonyms #-}
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

data Atom f = R f | V | Wk f | End
    deriving (Generic, Functor, Foldable, Traversable)


data Transaction u f 
    = TSend Participant u f 
    | TReceive Participant u f 
    deriving (Generic, Functor, Foldable, Traversable)

pattern BackwardSend participant tipe continuation = SendOrReceive (Transaction (TSend participant tipe ())) continuation
pattern BackwardReceive participant tipe continuation = SendOrReceive (Transaction (TReceive participant tipe ())) continuation

pattern Send participant tipe continuation = Transaction (TSend participant tipe continuation)
pattern Receive participant tipe continuation = Transaction (TReceive participant tipe continuation)

data LocalTypeF u f 
    = Transaction (Transaction u f)
    | Choice f f 
    | Offer f f
    | Atom (Atom f)
    deriving (Generic, Functor, Foldable, Traversable)

type LocalType u = Fix (LocalTypeF u)


type Crumb u = LocalTypeF u ()

type Identifier = String
type Location = String

-- T, S = hole | a.T | k.T | (l, l1, l2).T
data TypeContextF a f 
    = Hole 
    | SendOrReceive (LocalTypeF a ()) f 
    | Application Identifier f 
    | Spawning Location Location Location f
    | Assignment { visibleName :: Identifier, internalName :: Identifier, continuation :: f }
    | Literal a f
    deriving (Generic, Functor, Foldable, Traversable)


type TypeContext a = Fix (TypeContextF a)




unCrumb :: LocalType u -> LocalType.Crumb u -> LocalType u
unCrumb global crumb = 
    let levelUp = crumb 
    in
        Fix $ fmap (const global) levelUp

type LocalTypeState u =  ( TypeContext u, LocalType u ) -- ([LocalType.Crumb u], LocalType u)

projection :: GlobalType u -> Map Participant (LocalType u)
projection = undefined 


sendTransaction :: Participant -> u -> LocalType u -> LocalType u 
sendTransaction sender tipe = Fix . Transaction . TSend sender tipe

receiveTransaction :: Participant -> u -> LocalType u -> LocalType u 
receiveTransaction receiver tipe = Fix . Transaction . TReceive receiver tipe

end :: LocalType u
end = Fix . Atom $ End

recursive :: LocalType u -> LocalType u 
recursive = Fix . Atom . R

variable :: LocalType u 
variable = Fix . Atom $ V

broadenScopeOfRecursion :: LocalType u -> LocalType u
broadenScopeOfRecursion = Fix. Atom . Wk

projections :: GlobalType u -> Map Participant (LocalType u)
projections global = 
    GlobalType.participants global
        |> Set.foldr (\participant -> Map.insert participant (project participant global)) Map.empty 


project :: Participant -> GlobalType u -> LocalType u 
project participant = 
    Data.Fix.cata $ \global -> 
        case global of 
            GlobalType.Transaction sender receiver tipe cont -> 
                if participant == sender then 
                     sendTransaction sender tipe cont
                else if participant == receiver then 
                     receiveTransaction receiver tipe cont
                else 
                    cont 

            GlobalType.R nestedGlobalType -> 
                recursive nestedGlobalType 

            GlobalType.V -> 
                variable

            GlobalType.Wk nestedGlobalType -> 
                broadenScopeOfRecursion  nestedGlobalType

            GlobalType.End -> 
                end

