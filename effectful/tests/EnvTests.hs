module EnvTests (envTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Reader.Static
import Effectful.State.Static.Local
import qualified Utils as U

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "staircase forkEnv" test_staircaseForkEnv
  , testCase "tailEnv through forks" test_unforkedTailEnv
  , testCase "subsume works" test_subsumeEnv
  ]

test_staircaseForkEnv :: Assertion
test_staircaseForkEnv = runEff $ do
  unsafeEff $ \es0 -> do
    es0f <- forkEnv es0
    (`unEff` es0f) $ evalState s0 $ do
      unsafeEff $ \es1 -> do
        es1f <- forkEnv es1
        (`unEff` es1f) $ runReader () $ do
          U.assertEqual "expected result" s0 =<< get
  where
    s0 :: Int
    s0 = 1337

test_unforkedTailEnv :: Assertion
test_unforkedTailEnv = runEff . evalState s0 . runReader () $ do
  unsafeEff $ \es0 -> do
    es0f <- forkEnv es0
    (`unEff` es0f) $ runReader () $ do
      unsafeEff $ \es1 -> do
        es1f <- forkEnv es1
        (`unEff` es1f) $ runReader () . raise . raise . raise $ do
          U.assertEqual "expected result" s0 =<< get
  where
    s0 :: Int
    s0 = 1337

test_subsumeEnv :: Assertion
test_subsumeEnv = runEff $ do
  s <- execState @Int 0 . subsume @(State Int) $ do
    modify @Int (+1)
    raise $ modify @Int (+2)
  U.assertEqual "exepcted result" 3 s
