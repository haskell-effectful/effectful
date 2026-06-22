{-# LANGUAGE AllowAmbiguousTypes #-}
module OutputTests (outputTests) where

import Data.Foldable qualified as F
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.State.Static.Local
import Effectful.Output.Dynamic qualified as OD
import Effectful.Output.Static.Action qualified as OA
import Effectful.Output.Static.Local.Array qualified as OLA
import Effectful.Output.Static.Local.List qualified as OLL
import Effectful.Output.Static.Shared.Array qualified as OSA
import Effectful.Output.Static.Shared.List qualified as OSL
import Effectful.Labeled.Output qualified as LO
import Utils qualified as U

outputTests :: TestTree
outputTests = testGroup "Output"
  [ testCase "static local list" test_localList
  , testCase "static local array" test_localArray
  , testCase "static shared list" test_sharedList
  , testCase "static shared array" test_sharedArray
  , testCase "static action" test_action
  , testCase "dynamic dispatches to all backends" test_dynamic
  , testCase "labeled outputs are independent" test_labeled
  ]

test_localList :: Assertion
test_localList = runEff $ do
  (r, xs) <- OLL.runOutput $ do
    mapM_ OLL.output values
    pure "done"
  U.assertEqual "value" "done" r
  U.assertEqual "list" values xs

test_localArray :: Assertion
test_localArray = runEff $ do
  (r, arr) <- OLA.runOutput $ do
    mapM_ OLA.output values
    pure "done"
  U.assertEqual "value" "done" r
  U.assertEqual "array" values (F.toList arr)

test_sharedList :: Assertion
test_sharedList = runEff $ do
  (r, xs) <- OSL.runOutput $ do
    mapM_ OSL.output values
    pure "done"
  U.assertEqual "value" "done" r
  U.assertEqual "list" values xs

test_sharedArray :: Assertion
test_sharedArray = runEff $ do
  (r, arr) <- OSA.runOutput $ do
    mapM_ OSA.output values
    pure "done"
  U.assertEqual "value" "done" r
  U.assertEqual "array" values (F.toList arr)

test_action :: Assertion
test_action = runEff $ do
  (_, acc) <- runState @[Int] [] . OA.runOutput @Int (\o -> modify (o :)) $ do
    mapM_ OA.output values
  U.assertEqual "values fed in order" (reverse values) acc

test_dynamic :: Assertion
test_dynamic = runEff $ do
  (_, la) <- OD.runOutputLocalArray prog
  U.assertEqual "local array" values (F.toList la)
  (_, ll) <- OD.runOutputLocalList prog
  U.assertEqual "local list" values ll
  (_, sa) <- OD.runOutputSharedArray prog
  U.assertEqual "shared array" values (F.toList sa)
  (_, sl) <- OD.runOutputSharedList prog
  U.assertEqual "shared list" values sl
  (_, acc) <- runState @[Int] [] . OD.runOutputAction @Int (\o -> modify (o :)) $ prog
  U.assertEqual "action" (reverse values) acc
  where
    prog :: (HasCallStack, OD.Output Int :> es) => Eff es ()
    prog = mapM_ OD.output values

test_labeled :: Assertion
test_labeled = runEff $ do
  ((_, bs), as) <- LO.runOutputLocalList @"a" @Int . LO.runOutputLocalList @"b" @Int $ do
    LO.output @"a" @Int 1
    LO.output @"b" @Int 10
    LO.output @"a" @Int 2
    LO.output @"b" @Int 20
  U.assertEqual "label a" [1, 2] as
  U.assertEqual "label b" [10, 20] bs

-- | Emitting more values than the initial (zero) capacity exercises the array
-- growth path in the array backends.
values :: [Int]
values = [1 .. 10]
