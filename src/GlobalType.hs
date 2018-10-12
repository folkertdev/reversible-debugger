{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, DeriveGeneric, DeriveFunctor, DeriveTraversable, Rank2Types #-}
module GlobalType where 

import GHC.Generics 

import qualified Data.Foldable as Foldable
import Data.Set as Set
import Data.Map as Map (Map, elems, fromList, lookup)
import Data.Monoid ((<>), mconcat)
import qualified Data.List as List 

import Data.Void as Void (Void, absurd)
import Data.Fix
import Control.Monad.Free (Free(..), liftF)
import qualified Control.Monad.Free as Free 

import Utils ((|>), List)

{-| A global type with transaction and recursion -}
type TerminatingGlobalType participant u = Free (GlobalTypeF participant u) Void

type GlobalType participant u a = Free (GlobalTypeF participant u) a 

data GlobalTypeF participant u f
    = Transaction { from :: participant, to :: participant, tipe :: u, continuation ::  f } 
    | Choice { from :: participant, to :: participant, options :: Map String f }
    | R f
    | V
    | Wk f
    | End
    deriving (Show, Generic, Functor, Foldable, Traversable)

mapParticipants :: (p1 -> p2) -> GlobalType p1 u a -> GlobalType p2 u a
mapParticipants _ (Pure x) = Pure x
mapParticipants mapper (Free global) = Free $ 
    case global of 
        Transaction p1 p2 tipe cont -> 
            Transaction (mapper p1) (mapper p2) tipe $ mapParticipants mapper cont

        Choice p1 p2 conts -> 
            Choice (mapper p1) (mapper p2) $ fmap (mapParticipants mapper) conts 

        R cont -> 
            R $ mapParticipants mapper cont

        Wk cont -> 
            Wk $ mapParticipants mapper cont

        V -> 
            V 

        End -> 
            End

mapType :: (t1 -> t2) -> GlobalType p t1 a -> GlobalType p t2 a
mapType mapper (Pure x) = Pure x
mapType mapper (Free global) = Free $ 
    case global of 
        Transaction p1 p2 tipe cont -> 
            Transaction p1 p2 (mapper tipe) $ mapType mapper cont

        Choice p1 p2 conts -> 
            Choice p1 p2 $ fmap (mapType mapper) conts 

        R cont -> 
            R $ mapType mapper cont

        Wk cont -> 
            Wk $ mapType mapper cont

        V -> 
            V 

        End -> 
            End


message :: participant -> participant -> tipe -> GlobalType participant tipe () 
message from to tipe = liftF (Transaction from to tipe ())

messages :: participant -> List participant -> tipe -> GlobalType participant tipe () 
messages sender receivers tipe = go receivers 
  where go [] = Pure ()
        go (x:xs) = Free (Transaction sender x tipe $ go xs)

oneOf :: participant -> participant -> List (String, GlobalType participant u a) -> GlobalType participant u a 
oneOf selector offerer options = Free (Choice selector offerer (Map.fromList options))

recurse :: GlobalType p u a -> GlobalType p u a
recurse cont = Free (R cont)

broadenScope :: GlobalType p u a -> GlobalType p u a
broadenScope cont = Free (Wk cont)

weakenRecursion :: GlobalType p u a -> GlobalType p u a
weakenRecursion cont = Free (Wk cont)

recursionVariable :: GlobalType p u a 
recursionVariable = Free V

end :: TerminatingGlobalType p u  
end = Free End


{-| enumerate all the participants in a global type -}
participants :: Ord participant => TerminatingGlobalType participant u -> Set participant
participants = Free.iter helper . fmap Void.absurd 
  where helper global = 
            case global of 
                Transaction p1 p2 _ cont -> 
                    Set.fromList [ p1, p2 ] <> cont

                Choice p1 p2 conts -> 
                    Set.fromList [ p1, p2 ] <> mconcat (Map.elems conts)

                R cont -> 
                    cont

                Wk cont -> 
                    cont

                V -> 
                    Set.empty

                End -> 
                    Set.empty


