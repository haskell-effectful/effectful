module Main (main) where

import Test.Tasty

import AlternativeTests
import AsyncTests
import ConcurrencyTests
import EnvTests
import EnvironmentTests
import ErrorTests
import PrimTests
import ReaderTests
import StateTests
import TimeoutTests
import UnliftTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ alternativeTests
  , asyncTests
  , concurrencyTests
  , envTests
  , environmentTests
  , errorTests
  , primTests
  , readerTests
  , stateTests
  , timeoutTests
  , unliftTests
  ]
