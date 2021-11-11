module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import EnvironmentTests
import ErrorTests
import StateTests
import TimeoutTests
import UnliftTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , concurrencyTests
  , environmentTests
  , errorTests
  , stateTests
  , timeoutTests
  , unliftTests
  ]
