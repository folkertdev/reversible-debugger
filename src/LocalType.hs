{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, PatternSynonyms, DuplicateRecordFields #-}
module LocalType where 

import GHC.Generics

import Utils ((|>), List)
import Data.Map as Map 
import Data.Set as Set 
import Data.Fix
import Data.Map.Merge.Strict as Merge
import Zipper (Zipper)
import qualified Zipper

import GlobalType (GlobalType)
import qualified GlobalType

type Participant = String
type Identifier = String
type Location = String

{-| A local type with send, receive, recursion, choice and offer -}
type LocalType u = Fix (LocalTypeF u)

data Choice u f 
    = COffer { owner :: Participant, selector :: Participant, options :: Map String (LocalType u) }
    | CSelect { owner :: Participant, offerer :: Participant, options :: Map String (LocalType u) }
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data LocalTypeF u f 
    = Transaction (Transaction u f)
    | Choice (Choice u f)
    | Atom (Atom f)
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Atom f = R f | V | Wk f | End
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Transaction u f 
    = TSend { owner :: Participant, receiver :: Participant, tipe :: u, continuation :: f } 
    | TReceive { owner :: Participant, sender :: Participant, names :: Maybe (Identifier, Identifier), tipe ::  u, continuation :: f } 
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


-- PATTERNS

pattern BackwardSend owner participant tipe continuation = 
    SendOrReceive (Transaction (TSend owner participant tipe ())) continuation

pattern BackwardReceive owner participant visibleName variableName tipe continuation = 
    SendOrReceive (Transaction (TReceive owner participant (Just (visibleName, variableName)) tipe ())) continuation

pattern Send owner receiver tipe continuation = 
    Transaction (TSend owner receiver tipe continuation)

pattern Receive owner sender tipe continuation = 
    Transaction (TReceive owner sender Nothing tipe continuation)

pattern RecursionPoint rest = Atom (R rest)
pattern WeakenRecursion rest = Atom (Wk rest)
pattern RecursionVariable = Atom V

pattern BackwardRecursionPoint rest = SendOrReceive (Atom (R ())) rest
pattern BackwardWeakenRecursion rest = SendOrReceive (Atom (Wk ())) rest
pattern BackwardRecursionVariable rest = SendOrReceive (Atom V) rest 

pattern Offer  owner selector options = Choice (COffer owner selector options)
pattern Select owner offerer  options = Choice (CSelect owner offerer options)


{-| Data structure containing the information needed to roll an action -}
type TypeContext program value a = Fix (TypeContextF program value a)

data TypeContextF program value a f 
    = Hole 
    | SendOrReceive (LocalTypeF a ()) f 
    | Selected 
        { owner :: Participant
        , offerer :: Participant 
        , selection :: Zipper (String, value, program, LocalType a)
        , continuation :: f 
        }
    | Offered 
        { owner :: Participant
        , selector :: Participant 
        , picked :: Zipper (String, program, LocalType a)
        , continuation :: f 
        }
    | Application Identifier Identifier f 
    | Spawning Location Location Location f
    | Assignment { visibleName :: Identifier, internalName :: Identifier, continuation :: f }
    | Literal a f
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

backwardSend :: Participant ->  Participant -> u -> TypeContext p v u -> TypeContext p v u 
backwardSend owner sender tipe base = 
    let 
        -- local :: LocalTypeF u ()
        local = Transaction $ TSend owner sender tipe ()
    in
        Fix $ SendOrReceive local base


backwardReceive :: Participant ->  Participant -> u -> Identifier -> Identifier -> TypeContext p v u 
                -> TypeContext p v u 
backwardReceive owner sender tipe visibleName variableName base = 
    Fix $ BackwardReceive owner sender visibleName variableName tipe base  

backwardSelect :: Participant
               -> Participant
               -> Zipper (String, v, p, LocalType u) 
               -> TypeContext p v u 
               -> TypeContext p v u
backwardSelect owner offerer selection continuation =  
    Fix $ Selected owner offerer selection continuation   

backwardOffer :: Participant 
              -> Participant
              -> Zipper (String, p, LocalType u) 
              -> TypeContext p v u 
              -> TypeContext p v u
backwardOffer owner selector picked continuation = 
    Fix $ Offered owner selector picked continuation 


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

offer :: Participant -> Participant -> List (String, LocalType u) -> LocalType u 
offer owner offerer options = 
    Fix $ Offer owner offerer (Map.fromList options)

select :: Participant -> Participant -> List (String, LocalType u) -> LocalType u 
select owner selector options = 
    Fix $ Select owner selector (Map.fromList options)


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
                if sender == receiver then
                    if participant == sender then 
                         LocalType.send sender receiver tipe $ LocalType.receive receiver sender tipe cont
                    else 
                         cont
                else
                    if participant == sender then 
                         LocalType.send sender receiver tipe cont
                    else if participant == receiver then 
                         LocalType.receive receiver sender tipe cont
                    else 
                        cont 

            GlobalType.OneOf offerer selector options -> 
                if participant == offerer then 
                    LocalType.offer offerer selector (Map.toList options)
                else if participant == selector then 
                    LocalType.select selector offerer (Map.toList options)
                else 
                    LocalType.end 
            

            GlobalType.R nestedGlobalType -> 
                recurse nestedGlobalType 

            GlobalType.V -> 
                recursionVariable

            GlobalType.Wk nestedGlobalType -> 
                broadenScope nestedGlobalType

            GlobalType.End -> 
                end

