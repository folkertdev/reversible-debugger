{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, DeriveGeneric, DeriveFunctor, DeriveTraversable #-}
module GlobalType where 

import GHC.Generics 

import qualified Data.Foldable as Foldable
import Data.Set as Set
import Data.Monoid ((<>))

import Data.Fix

import Types ((|>))
-- import TypeState (Crumb(..))

type Participant = String

data GlobalTypeF u f
    = Transaction { from :: Participant, to :: Participant, tipe :: u, continuation ::  f } 
    | R f
    | V
    | Wk f
    | End
    deriving (Show, Generic, Functor, Foldable, Traversable)

data CrumbF s 
    = Before s
    | BeforeSend s 
    | AfterSend s 
    | AfterReceive s 
    | Chosen s s 
    | Offered s s
    deriving (Generic, Functor, Foldable, Traversable)

type Crumb u = CrumbF (GlobalTypeF u ())


unCrumb :: GlobalType u -> GlobalType.Crumb u -> GlobalType u
unCrumb global crumb = 
    let levelUp = foldl1 const crumb
    in
        Fix $ fmap (const global) levelUp

type List = []


type GlobalType u = Fix (GlobalTypeF u)


participants :: GlobalType u -> Set Participant
participants = 
    Data.Fix.cata $ \global ->
        case global of 
            Transaction p1 p2 _ cont -> 
                Set.fromList [ p1, p2 ] <> cont

            R cont -> 
                cont

            Wk cont -> 
                cont

            V -> 
                Set.empty

            End -> 
                Set.empty


type GlobalTypeState u =  (List (GlobalType.Crumb u), GlobalType u)


forgetState :: GlobalTypeState u -> GlobalType u 
forgetState (crumbs, left) = Foldable.foldr (flip unCrumb) left crumbs 

nested :: GlobalType u -> List (GlobalType u)
nested = Foldable.toList . unFix

end = Fix End

crumble :: GlobalType u -> ( GlobalType.Crumb u, GlobalType u ) 
crumble global = 
    case unFix global of 
        Transaction p1 p2 tipe cont -> 
            ( Before (Transaction p1 p2 tipe ())
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
        
