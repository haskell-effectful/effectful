{-# LANGUAGE AllowAmbiguousTypes #-}
module InputTests (inputTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.State.Static.Local
import Effectful.Input.Dynamic qualified as ID
import Effectful.Input.Static qualified as IS
import Effectful.Input.Static.Action qualified as IA
import Effectful.Labeled.Input qualified as LI
import Utils qualified as U

inputTests :: TestTree
inputTests = testGroup "Input"
  [ testCase "static" test_static
  , testCase "static action reruns the action" test_staticAction
  , testCase "dynamic (value)" test_dynamicValue
  , testCase "dynamic (action) reruns the action" test_dynamicAction
  , testCase "labeled inputs are independent" test_labeled
  ]

test_static :: Assertion
test_static = runEff . IS.runInput @Int 42 $ do
  U.assertEqual "input" 42 =<< IS.input @Int
  U.assertEqual "inputs" 43 =<< IS.inputs @Int (+ 1)

test_staticAction :: Assertion
test_staticAction = runEff . evalState @Int 0 . IA.runInput nextValue $ do
  U.assertEqual "1st input" 1 =<< IA.input @Int
  U.assertEqual "2nd input" 2 =<< IA.input @Int
  U.assertEqual "inputs" 3 =<< IA.inputs @Int id

test_dynamicValue :: Assertion
test_dynamicValue = runEff . ID.runInput @Int 42 $ do
  U.assertEqual "input" 42 =<< ID.input @Int
  U.assertEqual "inputs" 43 =<< ID.inputs @Int (+ 1)

test_dynamicAction :: Assertion
test_dynamicAction = runEff . evalState @Int 0 . ID.runInputAction nextValue $ do
  U.assertEqual "1st input" 1 =<< ID.input @Int
  U.assertEqual "2nd input" 2 =<< ID.input @Int
  U.assertEqual "inputs" 3 =<< ID.inputs @Int id

test_labeled :: Assertion
test_labeled = runEff
  . evalState @Int 0
  . LI.runInput @"a" @Int 1
  . LI.runInputAction @"b" nextValue
  $ do
    U.assertEqual "a" 1 =<< LI.input @"a" @Int
    U.assertEqual "inputs a" 11 =<< LI.inputs @"a" @Int (+ 10)
    U.assertEqual "1st b" 1 =<< LI.input @"b" @Int
    U.assertEqual "2nd b" 2 =<< LI.input @"b" @Int
    -- "a" is unaffected by reads of "b".
    U.assertEqual "a again" 1 =<< LI.input @"a" @Int

-- | An action that returns consecutive integers, so that re-running it is
-- observable.
nextValue :: State Int :> es => Eff es Int
nextValue = do
  modify @Int (+ 1)
  get
