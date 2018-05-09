{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, DeriveGeneric, DeriveFunctor, DeriveTraversable #-}
module GlobalType where 

import GHC.Generics 

import qualified Data.Foldable as Foldable
import Data.Set as Set
import Data.Map as Map (Map, elems, fromList, lookup)
import Data.Monoid ((<>), mconcat)
import qualified Data.List as List 

import Data.Void as Void (Void, absurd)
import Data.Fix
import Control.Monad.Free (Free(..))
import qualified Control.Monad.Free as Free 

import Utils ((|>), List)

{-| A global type with transaction and recursion -}
type GlobalType participant u = Fix (GlobalTypeF participant u)

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
mapParticipants mapper (Fix global) = Fix $ 
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
mapType mapper (Fix global) = Fix $ 
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


globalType :: Free (GlobalTypeF p u) Void -> GlobalType p u 
globalType (Pure v) = Void.absurd v
globalType (Free x) = Fix (fmap globalType x)


liftFree :: Functor f => f a -> Free f a
liftFree action = Free (fmap Pure action)

transaction :: (Show participant, Ord participant) => participant -> participant -> tipe -> IGlobalType participant tipe () 
transaction from to tipe = liftFree (Transaction from to tipe ())

transactions :: (Show participant, Ord participant) => participant -> List participant -> tipe -> IGlobalType participant tipe () 
transactions sender receivers tipe = 
    let
        go [] = Pure ()
        go (x:xs) = Free (Transaction sender x tipe $ go xs)
    in
        go receivers


oneOf :: (Show participant, Ord participant) => participant -> participant -> List (String, IGlobalType participant u a) -> IGlobalType participant u a
oneOf from to options = Free (Choice from to (Map.fromList options))

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
participants = 
    Data.Fix.cata $ \global ->
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




{-| A global type state containing information to go back (crumbs) and forward (GlobalType) -}
type GlobalTypeState p u =  (List (GlobalType.Crumb p u), GlobalType p u)

data CrumbF s 
    = Before s
    | BeforeSend s 
    | AfterSend s 
    | AfterReceive s 
    | Chosen s s 
    | Offered s s
    deriving (Generic, Functor, Foldable, Traversable)

type Crumb p u = CrumbF (GlobalTypeF p u ())


unCrumb :: GlobalType p u -> GlobalType.Crumb p u -> GlobalType p u
unCrumb global crumb = 
    let levelUp = foldl1 const crumb
    in
        Fix $ fmap (const global) levelUp



forgetState :: GlobalTypeState p u -> GlobalType p u 
forgetState (crumbs, left) = Foldable.foldr (flip unCrumb) left crumbs 

nested :: GlobalType p u -> List (GlobalType p u)
nested = Foldable.toList . unFix

{-
crumble :: GlobalType u -> ( GlobalType.Crumb u, GlobalType u ) 
crumble global = 
    case unFix global of 
        Transaction p1 p2 tipe cont -> 
            ( Before (Transaction p1 p2 tipe ())
            , cont 
            )

        OneOf p1 p2 conts -> 
            ( Before (OneOf p1 p2 conts) 
            , cont 
            )


        R cont -> 
            ( Before (R ())
            , cont 
            )

        Wk cont -> 
            ( Before (Wk ())
            , cont 
            )

        V -> 
            ( Before V
            , end 
            )

        End -> 
            ( Before End
            , end 
            )


forward :: GlobalTypeState u -> GlobalTypeState u
forward (crumbs, left) = 
    case crumbs of 
        [] ->  
            standard 

        (Before s : _ ) ->  
            standard 

        (BeforeSend s : rest ) ->  
            ( AfterSend s : rest, left ) 

        (AfterSend s : rest ) -> 
            ( AfterReceive s : rest, left )

        (AfterReceive s : rest ) -> 
            standard

        ( Chosen _ _ : _ ) -> 
            standard

        ( Offered _ _ : _ ) -> 
            standard


  where standard = 
            let ( crumb, cont ) = crumble left 
            in
                ( crumb : crumbs, cont )

backward :: GlobalTypeState u -> GlobalTypeState u 
backward ( crumbs, left ) = 
    case crumbs of 
        [] ->  
            ( [], left ) 

        (Before s : rest ) ->  
            ( rest, unCrumb left (Before s) ) 

        (BeforeSend s : rest ) ->  
            ( rest, unCrumb left (BeforeSend s) ) 

        (AfterReceive s : rest ) -> 
            ( AfterSend s : rest, left )

        (AfterSend s : rest ) -> 
            ( Before s : rest, left )

        ( Chosen p q : rest ) -> 
            ( rest, unCrumb left (Chosen p q) ) 

        ( Offered p q : rest ) -> 
            ( rest, unCrumb left (Offered p q) ) 
 -}       
