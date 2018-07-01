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
type GlobalType participant u = Free (GlobalTypeF participant u) Void

type IGlobalType participant u a = Free (GlobalTypeF participant u) a 

data GlobalTypeF participant u f
    = Transaction { from :: participant, to :: participant, tipe :: u, continuation ::  f } 
    | Choice { from :: participant, to :: participant, options :: Map String f }
    | R f
    | V
    | Wk f
    | End
    deriving (Show, Generic, Functor, Foldable, Traversable)

mapParticipants :: (p1 -> p2) -> GlobalType p1 a -> GlobalType p2 a
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

mapType :: (t1 -> t2) -> GlobalType p t1 -> GlobalType p t2
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



transaction :: (Show participant, Ord participant) => participant -> participant -> tipe -> IGlobalType participant tipe () 
transaction from to tipe = liftF (Transaction from to tipe ())

transactions :: (Show participant, Ord participant) => participant -> List participant -> tipe -> IGlobalType participant tipe () 
transactions sender receivers tipe = 
    let
        go [] = Pure ()
        go (x:xs) = Free (Transaction sender x tipe $ go xs)
    in
        go receivers


oneOf :: (Show participant, Ord participant) => participant -> participant -> List (String, IGlobalType participant u a) -> IGlobalType participant u a
oneOf selector offerer options = Free (Choice selector offerer (Map.fromList options))

recurse :: Free (GlobalTypeF p u) void -> Free (GlobalTypeF p u) void
recurse cont = Free (R cont)

broadenScope :: Free (GlobalTypeF p u) void -> Free (GlobalTypeF p u) void
broadenScope cont = Free (Wk cont)

weakenRecursion :: Free (GlobalTypeF p u) a -> Free (GlobalTypeF p u) a
weakenRecursion cont = Free (Wk cont)

recursionVariable :: Free (GlobalTypeF p u) a 
recursionVariable = Free V

end :: Free (GlobalTypeF p u) a 
end = Free End


{-| enumerate all the participants in a global type -}
participants :: Ord participant => GlobalType participant u -> Set participant
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


