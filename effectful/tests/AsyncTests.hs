module AsyncTests (asyncTests) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Set qualified as S
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Concurrent.Async
import Effectful.Error.Static
import Effectful.State.Dynamic

import Utils qualified as U

asyncTests :: TestTree
asyncTests = testGroup "Async"
  [ testCase "local state" test_localState
  , testCase "shared state" test_sharedState
  , testCase "error handling" test_errorHandling
  , testCase "async with unmask" test_asyncWithUnmask
  , testCase "pooled workers" test_pooledWorkers
  ]

test_localState :: Assertion
test_localState = runEff . runConcurrent . evalStateLocal x $ do
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
test_sharedState = runEff . runConcurrent . evalStateShared (S.empty @Int) $ do
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
test_errorHandling = runEff . runConcurrent . evalStateShared (0::Int) $ do
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

test_asyncWithUnmask :: Assertion
test_asyncWithUnmask = runEff . runConcurrent . evalStateLocal "initial" $ do
  x <- asyncWithUnmask $ \unmask -> do
    liftIO $ threadDelay 10000
    r1 <- get @String -- 2
    unmask $ put "unmask"
    r2 <- get @String -- 3
    pure (r1, r2)
  put "changed"  -- 1
  (inner1, inner2) <- wait x
  outer <- get  -- 4
  U.assertEqual "expected result"
    ("initial", "unmask", "changed")
    (inner1, inner2, outer)

test_pooledWorkers :: Assertion
test_pooledWorkers = runEff . runConcurrent . evalStateLocal (0::Int) $ do
  x <- pooledForConcurrentlyN threads [1..n] $ \k -> do
    r <- get @Int
    modify @Int (+1)
    pure $ k + r
  U.assertEqual "expected result" [1..n] x
  where
    n = 10
    threads = 4
