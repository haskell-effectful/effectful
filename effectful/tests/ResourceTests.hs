module ResourceTests
  ( resourceTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Effectful
import           Effectful.Resource
import           UnliftIO                       ( MonadUnliftIO
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )
import qualified Utils                         as U

resourceTests :: TestTree
resourceTests = testGroup
  "Resource"
  [ testCase "nested"          test_nested
  , testCase "useCurrentScope" test_useCurrentScope
  ]

test_nested :: Assertion
test_nested = runEff $ do
  rV <- newMVar []
  let step :: MonadUnliftIO m => Int -> m ()
      step n = modifyMVar_ rV (pure . (n :))
  runResource $ do
    step 1
    _ <- register (step 4)
    _ <- runResource $ do
      register (step 2)
    step 3
  r <- readMVar rV
  U.assertEqual "correct order" [1 .. 4] (reverse r)

test_useCurrentScope :: Assertion
test_useCurrentScope = runEff $ do
  rV <- newMVar []
  let step :: MonadUnliftIO m => Int -> m ()
      step n = modifyMVar_ rV (pure . (n :))
  runResource $ do
    s <- useCurrentScope
    step 1
    _ <- register (step 5)
    _ <- runResource $ do
      _ <- register (step 2)
      s (register (step 4))
    step 3
  r <- readMVar rV
  U.assertEqual "correct order" [1 .. 5] (reverse r)
