{-# LANGUAGE CPP #-}
module Concurrency (concurrencyBenchmark) where

#ifdef VERSION_criterion
import Criterion hiding (env)
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench hiding (env)
#endif

import Control.Concurrent.Async
import Control.Monad

import Effectful
import Effectful.Concurrent.Async qualified as A
import Effectful.Dispatch.Dynamic
import Utils

concurrencyBenchmark :: Benchmark
concurrencyBenchmark = bgroup "concurrency"
  [ bgroup "shallow" $ map shallowBench [1, 10, 100]
  , bgroup "deep" $ map deepBench [1, 10, 100]
  ]

shallowBench :: Int -> Benchmark
shallowBench n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (IO)" $ nfAppIO (asyncBenchIO n) op
  , bench "async (Eff)" $ nfAppIO (runShallow . A.runConcurrent . asyncBench n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runShallow . runFork1 . forkBench n) op
  , bench "Fork (localUnlift/withLiftMap)" $
    nfAppIO (runShallow . runFork2 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runShallow . runFork3 . forkBench n) op
  , bench "Fork (localLiftUnlift)" $
    nfAppIO (runShallow . runFork4 . forkBench n) op
  ]

deepBench :: Int -> Benchmark
deepBench n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (Eff)" $ nfAppIO (runDeep . A.runConcurrent . asyncBench n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runDeep . runFork1 . forkBench n) op
  , bench "Fork (localUnlift/withLiftMap)" $
    nfAppIO (runDeep . runFork2 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runDeep . runFork3 . forkBench n) op
  , bench "Fork (localLiftUnlift)" $
    nfAppIO (runDeep . runFork4 . forkBench n) op
  ]

op :: Monad m => m Int
op = pure 1

----------------------------------------

data Fork :: Effect where
  ForkWithUnmask :: ((forall a. m a -> m a) -> m r) -> Fork m (Async r)

type instance DispatchOf Fork = Dynamic

-- | Uses 'localUnliftIO' and 'withLiftMapIO'.
runFork1 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork1 = interpret $ \env -> \case
  ForkWithUnmask m -> withLiftMapIO env $ \liftMap -> do
    localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask

-- | Uses 'localUnlift' and 'withLiftMap'.
runFork2 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork2 = reinterpret A.runConcurrent $ \env -> \case
  ForkWithUnmask m -> withLiftMap env $ \liftMap -> do
    localUnlift env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
      A.asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask

-- | Uses 'localLiftUnliftIO'.
runFork3 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork3 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnliftIO env (ConcUnlift Persistent $ Limited 1) $ \lift unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift

-- | Uses 'localLiftUnlift'.
runFork4 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork4 = reinterpret A.runConcurrent $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnlift env (ConcUnlift Persistent $ Limited 1) $ \lift unlift -> do
      A.asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift

----------------------------------------

asyncBenchIO :: Int -> IO Int -> IO Int
asyncBenchIO n f = do
  a <- asyncWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  wait a
{-# NOINLINE asyncBenchIO #-}

asyncBench :: A.Concurrent :> es => Int -> Eff es Int -> Eff es Int
asyncBench n f = do
  a <- A.asyncWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  A.wait a
{-# NOINLINE asyncBench #-}

forkBench :: (IOE :> es, Fork :> es) => Int -> Eff es Int -> Eff es Int
forkBench n f = do
  a <- send $ ForkWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  liftIO $ wait a
{-# NOINLINE forkBench #-}
