module EnvironmentTests (environmentTests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Error (isDoesNotExistError)

import Effectful
import Effectful.Environment
import Utils qualified as U

environmentTests :: TestTree
environmentTests = testCaseSteps "Environment" $ \step -> do
  -- These tests need to be run sequentially because of
  -- https://github.com/haskell-effectful/effectful/issues/39.
  step "set and get an environment variable"
  test_setAndGet
  step "unset and try to get an environment variable"
  test_unsetAndGet
  step "execute effects with custom arguments"
  test_withArgs
  step "execute effects with custom program name"
  test_withProg

test_setAndGet :: Assertion
test_setAndGet = runEff . runEnvironment $ do
  let expected = "value"
      n = "EFFECTFUL_TEST1"
  setEnv n expected
  v <- getEnv n
  U.assertEqual "environment variable not set" expected v

test_unsetAndGet :: Assertion
test_unsetAndGet = runEff . runEnvironment $ do
  let n = "EFFECTFUL_TEST2"
  unsetEnv n
  U.assertThrows "environment variable is set" isDoesNotExistError (getEnv n)

test_withArgs :: Assertion
test_withArgs = runEff . runEnvironment $ do
  let expected = ["effectful-test"]
  args <- withArgs expected getArgs
  U.assertEqual "correct arguments" expected args

test_withProg :: Assertion
test_withProg = runEff . runEnvironment $ do
  let expected = "effectful-test"
  n <- withProgName expected getProgName
  U.assertEqual "correct program name" expected n
