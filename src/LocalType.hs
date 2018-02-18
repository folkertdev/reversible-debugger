{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable #-}
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
    = Send Participant u f 
    | Receive Participant u f 
    deriving (Generic, Functor, Foldable, Traversable)


data LocalTypeF u f 
    = Transaction (Transaction u f)
    | Choice f f 
    | Offer f f
    | Atom (Atom f)
    deriving (Generic, Functor, Foldable, Traversable)

type LocalType u = Fix (LocalTypeF u)

projection :: GlobalType u -> Map Participant (LocalType u)
projection = undefined 


sendTransaction :: Participant -> u -> LocalType u -> LocalType u 
sendTransaction sender tipe = Fix . Transaction . Send sender tipe

receiveTransaction :: Participant -> u -> LocalType u -> LocalType u 
receiveTransaction receiver tipe = Fix . Transaction . Receive receiver tipe

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

