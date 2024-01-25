module PrimTests (primTests) where

import Data.Primitive.MutVar
import Test.Tasty
import Test.Tasty.HUnit
import Utils qualified as U

import Effectful
import Effectful.Prim

primTests :: TestTree
primTests = testGroup "Prim"
  [ testCase "MutVar works" test_MutVar
  ]

test_MutVar :: Assertion
test_MutVar = runEff . runPrim $ do
  mv <- newMutVar (0::Int)
  modifyMutVar mv (+1)
  v <- readMutVar mv
  writeMutVar mv (v + 2)
  U.assertEqual "MutVar works" 3 =<< readMutVar mv
