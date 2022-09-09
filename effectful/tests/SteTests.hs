{-# LANGUAGE AllowAmbiguousTypes #-}
module SteTests (steTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.STE

steTests :: TestTree
steTests = testGroup "STE"
  [ testCase "MutVar works" test_MutVar
  ]

test_MutVar :: Assertion
test_MutVar = do
  assertEqual "MutVar works" 3 (runPureEff $ runSTE f)
  where
    f :: forall s es. STE s :> es => Eff es Int
    f = do
      mv <- newMutVar @s (0::Int)
      modifyMutVar mv (+1)
      v <- readMutVar mv
      writeMutVar mv (v + 2)
      readMutVar mv
