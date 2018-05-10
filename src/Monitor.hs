{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Monitor where 

import Data.Map as Map

import LocalType (LocalTypeState, LocalType, Participant, Location, Identifier)
import Program (Program, terminate, Value(..), IntOperator(..))
import Queue (Queue, QueueError)
import qualified Queue
import Utils (List)


data Monitor value tipe = 
    Synchronized 
        { _localType :: LocalTypeState (Program value) value tipe
        , _recursiveVariableNumber :: Int
        , _recursionPoints :: List (LocalType tipe)
        , _store :: Map Identifier value 
        , _applicationHistory :: Map Identifier (Identifier, value)
        }
    | Unsynchronized
        { _localType :: LocalTypeState (Program value) value tipe
        , _recursiveVariableNumber :: Int
        , _recursionPoints :: List (LocalType tipe)
        , _store :: Map Identifier value 
        , _applicationHistory :: Map Identifier (Identifier, value)
        }
        deriving (Show, Eq)

