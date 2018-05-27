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
    | Atom (Atom f)
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Atom f = R f | V | Wk f | End
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


data Transaction u f 
    = TSend { owner :: Participant, receiver :: Participant, tipe :: u, continuation :: f } 
    | TReceive { owner :: Participant, sender :: Participant, tipe ::  u, continuation :: f } 
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


-- PATTERNS

pattern BackwardSend owner participant tipe continuation = 
    LocalType (Transaction (TSend owner participant tipe ())) continuation

pattern BackwardReceive owner participant tipe continuation = 
    LocalType (Transaction (TReceive owner participant tipe ())) continuation

pattern Send owner receiver tipe continuation = 
    Transaction (TSend owner receiver tipe continuation)

pattern Receive owner sender tipe continuation = 
    Transaction (TReceive owner sender tipe continuation)

pattern RecursionPoint rest = Atom (R rest)
pattern WeakenRecursion rest = Atom (Wk rest)
pattern RecursionVariable = Atom V

pattern BackwardRecursionPoint rest = LocalType (Atom (R ())) rest
pattern BackwardWeakenRecursion rest = LocalType (Atom (Wk ())) rest
pattern BackwardRecursionVariable rest = LocalType (Atom V) rest 

pattern Offer  owner selector options = Choice (COffer owner selector options)
pattern Select owner offerer  options = Choice (CSelect owner offerer options)



{-| Data structure containing the information needed to roll an action -}
type TypeContext program value a = Fix (TypeContextF program value a)

data TypeContextF program value a f 
    = Hole 
    | LocalType (LocalTypeF a ()) f 
    | Selected 
        { owner :: Participant
        , offerer :: Participant 
        , selection :: Zipper (String, LocalType a)
        , continuation :: f 
        }
    | Offered 
        { owner :: Participant
        , selector :: Participant 
        , picked :: Zipper (String, LocalType a)
        , continuation :: f 
        }
    | Branched { continuation :: f }
    | Application Participant Identifier f 
    | Spawning Location Location Location f
    | Assignment { owner :: Participant, continuation :: f }
    | Literal a f
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

backwardSend :: Participant ->  Participant -> u -> TypeContext p v u -> TypeContext p v u 
backwardSend owner sender tipe base = 
    let 
        -- local :: LocalTypeF u ()
        local = Transaction $ TSend owner sender tipe ()
    in
        Fix $ LocalType local base


backwardReceive :: Participant ->  Participant -> u -> TypeContext p v u 
                -> TypeContext p v u 
backwardReceive owner sender tipe base = 
    Fix $ BackwardReceive owner sender tipe base  

backwardSelect :: Participant
               -> Participant
               -> Zipper (String, LocalType u) 
               -> TypeContext p v u 
               -> TypeContext p v u
backwardSelect owner offerer selection continuation =  
    Fix $ Selected owner offerer selection continuation   

backwardOffer :: Participant 
              -> Participant
              -> Zipper (String, LocalType u) 
              -> TypeContext p v u 
              -> TypeContext p v u
backwardOffer owner selector picked continuation = 
    Fix $ Offered owner selector picked continuation 


{-| The state of a local type is 
 * the information needed to roll, the TypeContext
 * the information needed to step forward, the LocalType
-}
type LocalTypeState program value u  = Synchronizable ( TypeContext program value u, LocalType u ) 


data Synchronizable a = Synchronized a | Unsynchronized a deriving (Show, Eq, Functor)

createState :: TypeContext program value u -> LocalType u -> LocalTypeState program value u
createState = curry Unsynchronized 

unwrapState state = 
    case state of 
        Unsynchronized x ->  
            x

        Synchronized x -> 
            x

mapState :: (TypeContext program value u -> LocalType u -> (TypeContext program value u, LocalType u)) -> LocalTypeState program value u -> LocalTypeState program value u 
mapState tagger = fmap (uncurry tagger) 

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


                        -- combined = List.foldl (\accum elem options -> elem [("multicast", accum options)]) () mappers

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

