{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module NestedDelegation where

import           Control.Monad.State as State

import qualified GlobalType
import           LocalType           (Identifier, LocalType, LocalTypeState,
                                      Location, Participant, Transaction (..))
import qualified LocalType

import           Data.Fix            as Fix
import           Data.Map            as Map (Map)
import qualified Data.Map            as Map

import           Debug.Trace         as Debug
import           Types               ((|>))

import           Semantics           (ExecutionState (..), List, Monitor (..),
                                      Program, ProgramF (..), Session,
                                      Value (..), applyFunction, emptyQueue,
                                      forward_, receive, send, terminate, run)

globalType :: GlobalType.GlobalType String
globalType =
  GlobalType.transaction "A" "V" "title" $
  GlobalType.transaction "V" "A" "price" $
  GlobalType.transaction "V" "B" "price" $
  GlobalType.transaction "A" "B" "share" $
  GlobalType.transaction "B" "A" "ok" $
  GlobalType.transaction "B" "V" "ok" $
  GlobalType.transaction "B" "C" "share" $
  GlobalType.transaction "B" "C" "thunk" $
  GlobalType.transaction "C" "D" "thunk" $
  GlobalType.transaction "B" "V" "address" $
  GlobalType.transaction "V" "B" "date" $ 
  GlobalType.end

localTypes :: Map Identifier (LocalType.LocalType String)
localTypes = LocalType.projections globalType

alice =
  let h = VInt 42
  in 
    run 
        [ send "A" (VString "Logicomix")
        , receive "A" "p" 
        , send "A" h 
        , receive "A" "ok" 
        ]

bob =
  let thunk =
        VFunction "_" (send "B" (VString "Lucca, 55100") $ receive "B" "d" terminate)

      ok = VBool True
  in 
    run 
        [ receive "B" "p"
        , receive "B" "h"
        , send "B" ok
        , send "B" ok
        , send "B" (VReference "h") 
        , send "B" thunk
        ] 


carol =
    run
        [ receive "C" "h"
        , receive "C" "code"
        , send "C" (VReference "code")
        ]


dave = 
    receive "D" "code" $
    applyFunction "code" VUnit


vendor =
  let price title = VInt 42
      date = VString "2018-03-14"
  in 
    run 
        [ receive "V" "t"
        , send "V" (price (VReference "t"))
        , send "V" (price (VReference "t"))
        , receive "V" "ok" 
        , receive "V" "a" 
        , send "V" date 
        ]

executionState =
  let createMonitor participant =
        Monitor
        { _localType = (Fix LocalType.Hole, localTypes Map.! participant)
        , _freeVariables = []
        , _store = Map.empty
        , _applicationHistory = Map.empty
        , _reversible = False
        , _recursiveVariableNumber = 0
        , _recursionPoints = []
        }
  in ExecutionState
     { variableCount = 0
     , locationCount = 1
     , applicationCount = 0
     , participants =
         Map.fromList
           [ ("A", createMonitor "A")
           , ("B", createMonitor "B")
           , ("C", createMonitor "C")
           , ("V", createMonitor "V")
           , ("D", createMonitor "D")
           ]
     , locations =
         [("A", alice), ("B", bob), ("C", carol), ("V", vendor), ("D", dave)]
            |> Map.fromList
            |> Map.singleton "Location1"
     , queue = emptyQueue
     , isFunction =
         \value ->
           case value of
             VFunction arg body -> Just (arg, body)
             _                  -> Nothing
     }

steps =
  flip State.runStateT executionState $ do
    forward_ "Location1" "A"
    forward_ "Location1" "V"

    forward_ "Location1" "V"
    forward_ "Location1" "A"

    forward_ "Location1" "V"
    forward_ "Location1" "B"

    forward_ "Location1" "A"
    forward_ "Location1" "B"

    forward_ "Location1" "B"
    forward_ "Location1" "A"

    forward_ "Location1" "B"
    forward_ "Location1" "V"

    forward_ "Location1" "B"
    forward_ "Location1" "C"
            -- send thunk from B to C
    forward_ "Location1" "B"
    forward_ "Location1" "C"
            -- send thunk from C to D
    forward_ "Location1" "C"
    forward_ "Location1" "D"
            -- force thunk
    forward_ "Location1" "D"
            -- evaluate the thunk
    forward_ "Location1" "D"
    forward_ "Location1" "V"
    forward_ "Location1" "V"
    forward_ "Location1" "D"
