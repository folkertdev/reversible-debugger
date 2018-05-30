{-# LANGUAGE ScopedTypeVariables, DuplicateRecordFields, DeriveGeneric, DeriveFunctor, DeriveTraversable #-}
module TypeState where

import GHC.Generics

data Crumb f 
    = Before f 
    | BeforeSend f
    | AfterSend f
    | AfterReceive f
    | Chosen { chosen :: f, otherOption :: f }
    | Offered { offered :: f, otherOption :: f } 
    deriving (Generic, Functor, Foldable, Traversable)

