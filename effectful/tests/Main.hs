module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import EarlyReturnTests
import EnvTests
import EnvironmentTests
import ErrorTests
import LabeledTests
import NonDetTests
import PrimTests
import ReaderTests
import StateTests
import TimeoutTests
import UnliftTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , concurrencyTests
  , earlyReturnTests
  , envTests
  , environmentTests
  , errorTests
  , labeledTests
  , nonDetTests
  , primTests
  , readerTests
  , stateTests
  , timeoutTests
  , unliftTests
  ]
