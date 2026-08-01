module Main (main) where

import Test.Tasty

import AsyncTests
import ConcurrencyTests
import EnvTests
import EnvironmentTests
import ErrorTests
import ExceptionTests
import FileSystemTests
import InputTests
import LabeledTests
import NonDetTests
import OutputTests
import PrimTests
import ReaderTests
import ReturnWithTests
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
  , exceptionTests
  , fileSystemTests
  , inputTests
  , labeledTests
  , nonDetTests
  , outputTests
  , primTests
  , readerTests
  , returnWithTests
  , stateTests
  , timeoutTests
  , unliftTests
  , word64MapTests
  ]
