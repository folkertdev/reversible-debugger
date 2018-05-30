{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, PatternSynonyms, DuplicateRecordFields #-}
module TypeContext 
    (TypeContext
    , pattern Offered
    , pattern Selected
    , pattern Sent
    , pattern Received
    , pattern RecursionVariable
    , pattern RecursionPoint 
    , pattern WeakenRecursion 
    , pattern Application
    , pattern Assigned
    , pattern Branched
    , pattern Spawned
    , pattern Hole
    , LocalTypeState
    , Synchronizable(..)
    , unwrapState
    , createState
    , mapState
    ) 
      where 

import Data.Fix
import Zipper

import qualified LocalType
import LocalType (LocalType, Location, Participant, Identifier)

import GHC.Generics

{-| Data structure containing the information needed to roll an action -}
type TypeContext a = Fix (TypeContextF a)

data TypeContextF a previous 
    = IHole 
    | Transaction (LocalType.Transaction a previous)
    | IR previous  
    | IW previous
    | IV previous
    | ISelected 
        { owner :: Participant
        , offerer :: Participant 
        , selection :: Zipper (String, LocalType a)
            , continuation :: previous 
        }
    | IOffered 
        { owner :: Participant
        , selector :: Participant 
        , picked :: Zipper (String, LocalType a)
            , continuation :: previous 
        }
    | IBranched { owner :: Participant, continuation :: previous }
    | IApplication Participant Identifier previous 
    | ISpawning Location Location Location previous
    | IAssignment { owner :: Participant, continuation :: previous }
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

pattern Hole = 
    Fix IHole

pattern Sent owner participant tipe continuation = 
    Fix (Transaction (LocalType.TSend owner participant tipe continuation))

pattern Received owner participant tipe continuation = 
    Fix (Transaction (LocalType.TReceive owner participant tipe continuation))

pattern Offered owner selector picked continuation =
    Fix (IOffered owner selector picked continuation)

pattern Selected owner offerer selection continuation =
    Fix (ISelected owner offerer selection continuation)

pattern Branched owner continuation = 
    Fix (IBranched owner continuation)

pattern Application owner k continuation = 
    Fix (IApplication owner k continuation)

pattern Assigned owner continuation = 
    Fix (IAssignment owner continuation)

pattern Spawned l1 l2 l3 continuation = 
    Fix (ISpawning l1 l2 l3 continuation)


pattern RecursionPoint previous = Fix (IR previous)
pattern WeakenRecursion previous = Fix (IW previous)
pattern RecursionVariable previous = Fix (IV previous) 

-- TYPE STATE

{-| The state of a local type is 
 * the information needed to roll, the TypeContext
 * the information needed to step forward, the LocalType
-}
type LocalTypeState u  = Synchronizable ( TypeContext u, LocalType u ) 


data Synchronizable a = Synchronized a | Unsynchronized a deriving (Show, Eq, Functor)

createState :: TypeContext u -> LocalType u -> LocalTypeState u
createState = curry Unsynchronized 

unwrapState state = 
    case state of 
        Unsynchronized x ->  
            x

        Synchronized x -> 
            x

mapState :: (TypeContext u -> LocalType u -> (TypeContext u, LocalType u)) -> LocalTypeState u -> LocalTypeState u 
mapState tagger = fmap (uncurry tagger) 
