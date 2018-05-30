{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveFunctor, DeriveTraversable, PatternSynonyms, DuplicateRecordFields #-}
module TypeContext where 

import Data.Fix
import Zipper

import qualified LocalType
import LocalType (LocalType, Location, Participant, Identifier)

import GHC.Generics

{-| Data structure containing the information needed to roll an action -}
type TypeContext a = Fix (TypeContextF a)

data TypeContextF a f 
    = Hole 
    | Transaction (LocalType.Transaction a f)
    | IRecursion (LocalType.Recursion a f)
    | ISelected 
        { owner :: Participant
        , offerer :: Participant 
        , selection :: Zipper (String, LocalType a)
        , continuation :: f 
        }
    | IOffered 
        { owner :: Participant
        , selector :: Participant 
        , picked :: Zipper (String, LocalType a)
        , continuation :: f 
        }
    | IBranched { owner :: Participant, continuation :: f }
    | IApplication Participant Identifier f 
    | ISpawning Location Location Location f
    | IAssignment { owner :: Participant, continuation :: f }
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)


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

pattern Application owner continuation = 
    Fix (IApplication owner continuation)

pattern Assigned owner continuation = 
    Fix (IAssignment owner continuation)

pattern Spawned l1 l2 l3= 
    Fix (ISpawning l1 l2 l3)

pattern Recursion a = 
    Fix (Recursion a)

pattern RecursionPoint rest = Recursion (LocalType.R rest)
pattern WeakenRecursion rest = Recursion (LocalType.Wk rest)
pattern RecursionVariable = Recursion LocalType.V

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
