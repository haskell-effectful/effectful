module ConcurrencyTests (concurrencyTests) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as S
import UnliftIO

import Effectful
import Effectful.Error
import Effectful.State.Dynamic

import qualified Utils as U

concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency"
  [ testCase "local state" test_localState
  , testCase "shared state" test_sharedState
  , testCase "error handling" test_errorHandling
  ]

test_localState :: Assertion
test_localState = runIOE . evalState x $ do
  replicateConcurrently_ 2 $ do
    r <- goDownward 0
    U.assertEqual "expected result" x r
  where
    x :: Int
    x = 100000

    goDownward :: State Int :> es => Int -> Eff es Int
    goDownward acc = do
      end <- state @Int $ \case
        0 -> (True,  0)
        n -> (False, n - 1)
      if end
        then pure acc
        else goDownward $ acc + 1

test_sharedState :: Assertion
test_sharedState = runIOE . evalStateMVar (S.empty @Int) $ do
  concurrently_ (addWhen even x) (addWhen odd x)
  U.assertEqual "expected result" (S.fromList [1..x]) =<< get
  where
    x :: Int
    x = 100

    addWhen :: State (S.Set Int) :> es => (Int -> Bool) -> Int -> Eff es ()
    addWhen f = \case
      0 -> pure ()
      n -> do
        when (f n) $ do
          modify $ S.insert n
        addWhen f $ n - 1

test_errorHandling :: Assertion
test_errorHandling = runIOE . evalStateMVar (0::Int) $ do
  r <- runError $ concurrently_
    (liftIO (threadDelay 10000) >> throwError err)
    (modify (+x))
  case r of
    Left (_, e) -> U.assertEqual "error caught" err e
    Right _     -> U.assertFailure "error not caught"
  U.assertEqual "state updated" x =<< get
  where
    x :: Int
    x = 67

    err :: String
    err = "thrown from async"
