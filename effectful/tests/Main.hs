module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import EnvTests
import EnvironmentTests
import ErrorTests
import ReaderTests
import StateTests
import TimeoutTests
import UnliftTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , concurrencyTests
  , envTests
  , environmentTests
  , errorTests
  , readerTests
  , stateTests
  , timeoutTests
  , unliftTests
  ]
