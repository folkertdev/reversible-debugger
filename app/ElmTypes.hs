{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics
import Data.Proxy
import Elm


import qualified Data.Text as Text
import Data.Monoid ((<>))


import Types
import SessionType
import Queue
import MicroOz
import Data.Context
import Data.Thread
import Data.ThreadState
import Data.ReplState
import Data.PID
import DebuggerParser
import Data.Expr (IntExpr, BoolExpr)



toBoth p = toElmTypeSource p <> "\n\n" <> toElmDecoderSource p <> "\n\n" <> toElmEncoderSource p



types :: Spec
types =
  Spec
    [ "TypesGenerated"]
    [ "import Json.Decode exposing (..)"
    , "import Dict exposing (Dict)" 
    , "import Json.Decode.Pipeline exposing (..)"
    , toBoth (Proxy :: Proxy (LocalAtom String))
    {-
    , toBoth (Proxy :: Proxy (Result Int String))
    , toBoth (Proxy :: Proxy Instruction)
    , toBoth (Proxy :: Proxy IntExpr)
    , toBoth (Proxy :: Proxy BoolExpr)
    , toBoth (Proxy :: Proxy ReplState)
    , toBoth (Proxy :: Proxy (Context Value))
    , toBoth (Proxy :: Proxy (OtherThreads History Value))
    , toBoth (Proxy :: Proxy (ThreadState History Value))
    , toBoth (Proxy :: Proxy (Thread History Value))
    , toBoth (Proxy :: Proxy Identifier)
    , toBoth (Proxy :: Proxy PID)
    , toBoth (Proxy :: Proxy History)

    , toBoth (Proxy :: Proxy Value)
    , toBoth (Proxy :: Proxy BooleanOperator)
    , toBoth (Proxy :: Proxy IntOperator)

    , toBoth (Proxy :: Proxy Program)

    , toBoth (Proxy :: Proxy (Queue Identifier))
    , toBoth (Proxy :: Proxy (Queue.Item Identifier))
    , toBoth (Proxy :: Proxy Queue.QueueHistory)

    , toBoth (Proxy :: Proxy (LocalTypeState String))
    , toBoth (Proxy :: Proxy (ExhaustableZipper String))
    -}
    ]

main :: IO ()
main = specsToDir [types] "frontend/src"
