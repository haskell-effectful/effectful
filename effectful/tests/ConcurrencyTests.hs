module ConcurrencyTests (concurrencyTests) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import qualified Data.Set as S

import Effectful
import Effectful.Error
import Effectful.State.Dynamic
import qualified Utils as U

concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency"
  [ testCase "local state" test_localState
  , testCase "shared state" test_sharedState
  , testCase "error handling" test_errorHandling
  , testCase "unlifting several times" test_unliftMany
  , testCase "async with unmask" test_asyncWithUnmask
  , testCase "pooled workers" test_pooledWorkers
  ]

test_localState :: Assertion
test_localState = runEff . evalLocalStateE x $ do
  withUnliftStrategy (ConcUnlift Ephemeral $ Limited 2) $ do
    replicateConcurrently_ 2 $ do
      r <- goDownward 0
      U.assertEqual "expected result" x r
  where
    x :: Int
    x = 100000

    goDownward :: StateE Int :> es => Int -> Eff es Int
    goDownward acc = do
      end <- state @Int $ \case
        0 -> (True,  0)
        n -> (False, n - 1)
      if end
        then pure acc
        else goDownward $ acc + 1

test_sharedState :: Assertion
test_sharedState = runEff . evalSharedStateE (S.empty @Int) $ do
  withUnliftStrategy (ConcUnlift Ephemeral $ Limited 2) $ do
    concurrently_ (addWhen even x) (addWhen odd x)
    U.assertEqual "expected result" (S.fromList [1..x]) =<< get
  where
    x :: Int
    x = 100

    addWhen :: StateE (S.Set Int) :> es => (Int -> Bool) -> Int -> Eff es ()
    addWhen f = \case
      0 -> pure ()
      n -> do
        when (f n) $ do
          modify $ S.insert n
        addWhen f $ n - 1

test_errorHandling :: Assertion
test_errorHandling = runEff . evalSharedStateE (0::Int) $ do
  withUnliftStrategy (ConcUnlift Ephemeral $ Limited 2) $ do
    r <- runErrorE $ concurrently_
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

test_unliftMany :: Assertion
test_unliftMany = runEff . evalLocalStateE "initial value" $ do
  withUnliftStrategy (ConcUnlift Persistent $ Limited 1) $ do
    x <- withRunInIO $ \runInIO -> async $ do
      v1 <- runInIO $ get @String  -- 1
      threadDelay 20000
      v2 <- runInIO $ get @String -- 3
      runInIO $ put "inner change"
      v3 <- runInIO $ get @String
      return (v1, v2, v3)
    liftIO $ threadDelay 10000
    put "outer change"  -- 2
    (v1, v2, v3) <- liftIO $ wait x
    v4 <- get  -- 4
    U.assertEqual "expected result"
      ("initial value", "initial value", "inner change", "outer change")
      (v1, v2, v3, v4)

test_asyncWithUnmask :: Assertion
test_asyncWithUnmask = runEff . evalLocalStateE "initial" $ do
  withUnliftStrategy (ConcUnlift Persistent $ Limited 1) $ do
    x <- asyncWithUnmask $ \unmask -> do
      liftIO $ threadDelay 10000
      r1 <- get @String -- 2
      unmask $ put "unmask"
      r2 <- get @String -- 3
      pure (r1, r2)
    put "changed"  -- 1
    (inner1, inner2) <- liftIO $ wait x
    outer <- get  -- 4
    U.assertEqual "expected result"
      ("initial", "unmask", "changed")
      (inner1, inner2, outer)

test_pooledWorkers :: Assertion
test_pooledWorkers = runEff . evalLocalStateE (0::Int) $ do
  withUnliftStrategy (ConcUnlift Ephemeral $ Limited n) $ do
    x <- pooledForConcurrentlyN threads [1..n] $ \k -> do
      r <- get @Int
      modify @Int (+1)
      pure $ k + r
    U.assertEqual "expected result" [1..n] x
  where
    n = 10
    threads = 4
