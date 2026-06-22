module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import EnvTests
import EnvironmentTests
import ErrorTests
import InputTests
import LabeledTests
import NonDetTests
import OutputTests
import PrimTests
import ReaderTests
import StateTests
import TimeoutTests
import UnliftTests
import Word64MapTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , concurrencyTests
  , envTests
  , environmentTests
  , errorTests
  , inputTests
  , labeledTests
  , nonDetTests
  , outputTests
  , primTests
  , readerTests
  , stateTests
  , timeoutTests
  , unliftTests
  , word64MapTests
  ]
