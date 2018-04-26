module Main where

import Semantics

import Examples.Recursive
import Examples.ParallelDelegation
import Examples.NestedDelegation

main = print $ Examples.NestedDelegation.steps 
