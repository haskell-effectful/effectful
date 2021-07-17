module Main (main) where

import Test.Tasty

import ConcurrencyTests
import ErrorTests
import StateTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ concurrencyTests
  , errorTests
  , stateTests
  ]
