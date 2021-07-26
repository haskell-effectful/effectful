module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import ErrorTests
import ResourceTests
import StateTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , concurrencyTests
  , errorTests
  , resourceTests
  , stateTests
  ]
