{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, PatternSynonyms, DuplicateRecordFields #-}
module LocalType where 

import GHC.Generics

import Utils ((|>), List)
import Data.Map as Map 
import Data.List as List
import Data.Set as Set 
import Data.Fix
import Data.Map.Merge.Strict as Merge
import Zipper (Zipper)
import qualified Zipper

import GlobalType (GlobalType, GlobalTypeF)
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
    | Recursion (Recursion f)
    | End
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Recursion f = R f | V | Wk f 
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Transaction u f 
    = TSend { owner :: Participant, receiver :: Participant, tipe :: u, continuation :: f } 
    | TReceive { owner :: Participant, sender :: Participant, tipe ::  u, continuation :: f } 
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


-- PATTERNS

pattern Send owner receiver tipe continuation = 
    Transaction (TSend owner receiver tipe continuation)

pattern Receive owner sender tipe continuation = 
    Transaction (TReceive owner sender tipe continuation)

pattern RecursionPoint rest = Recursion (R rest)
pattern WeakenRecursion rest = Recursion (Wk rest)
pattern RecursionVariable = Recursion V

pattern Offer  owner selector options = Choice (COffer owner selector options)
pattern Select owner offerer  options = Choice (CSelect owner offerer options)


recurse :: LocalType a -> LocalType a 
recurse cont = Fix . Recursion $ R cont

broadenScope :: LocalType a -> LocalType a 
broadenScope cont = Fix . Recursion $ Wk cont

recursionVariable :: LocalType a
recursionVariable = Fix $ Recursion V

end :: LocalType a
end = Fix End

send :: Participant ->  Participant -> u -> LocalType u -> LocalType u 
send owner sender tipe = Fix . Transaction . TSend owner sender tipe


receive :: Participant -> Participant -> u -> LocalType u -> LocalType u 
receive owner receiver tipe = Fix . Transaction . TReceive owner receiver tipe

offer :: Participant -> Participant -> List (String, LocalType u) -> LocalType u 
offer owner offerer options = 
    Fix $ Offer owner offerer (Map.fromList options)

select :: Participant -> Participant -> List (String, LocalType u) -> LocalType u 
select owner selector options = 
    Fix $ Select owner selector (Map.fromList options)


projections :: forall participant u. (Show participant, Ord participant) => GlobalType participant u -> Map Participant (LocalType u)
projections global = 
    let withStringParticipants :: GlobalType Participant u
        withStringParticipants =  global |> GlobalType.mapParticipants show 

        participants = 
            withStringParticipants 
                |> GlobalType.participants 
    in
        participants 
            |> Set.foldr (\participant -> Map.insert participant (project participants participant withStringParticipants)) Map.empty 


{-| Project a global type into a local one for a particular participant -}
project :: Set Participant -> Participant -> GlobalType Participant u -> LocalType u 
project participants participant = 
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

            GlobalType.Choice selector offerer options -> 
                if participant == offerer then 
                    let others = Set.toList $ Set.difference participants (Set.fromList [ selector, offerer ]) 

                        folder participant (label, accum) = 
                            ( label, LocalType.select offerer participant [ (label, accum) ] )

                        f (label, tipe ) = 
                            List.foldr folder (label, tipe) others 

                        newOptions = List.map f $ Map.toList options
                    in
                        LocalType.offer offerer selector newOptions

                else if participant == selector then 
                    LocalType.select selector offerer (Map.toList options)
                else 
                    -- we cast the decision to everyone
                    LocalType.offer participant offerer (Map.toList options)

            

            GlobalType.R nestedGlobalType -> 
                recurse nestedGlobalType 

            GlobalType.V -> 
                recursionVariable

            GlobalType.Wk nestedGlobalType -> 
                broadenScope nestedGlobalType

            GlobalType.End -> 
                end

