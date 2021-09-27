module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import ErrorTests
import StateTests
import TimeoutTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , concurrencyTests
  , errorTests
  , stateTests
  , timeoutTests
  ]
