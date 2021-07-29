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
import Control.Monad.IO.Class
import qualified Effectful.Async as A

import Effectful
import Utils

concurrencyBenchmark :: Benchmark
concurrencyBenchmark = bgroup "concurrency"
  [ bgroup "shallow" $ map shallowBench [1, 10, 100]
  , bgroup "deep" $ map deepBench [1, 10, 100]
  ]

shallowBench :: Int -> Benchmark
shallowBench n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (IO)" $ nfAppIO (asyncBenchIO n) op
  , bench "async (Eff)" $ nfAppIO (runShallow . A.runAsyncE . asyncBench n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runShallow . runForkE1 . forkBench n) op
  , bench "Fork (localUnlift/withLiftMap)" $
    nfAppIO (runShallow . runForkE2 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runShallow . runForkE3 . forkBench n) op
  , bench "Fork (localLiftUnlift)" $
    nfAppIO (runShallow . runForkE4 . forkBench n) op
  ]

deepBench :: Int -> Benchmark
deepBench n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (Eff)" $ nfAppIO (runDeep . A.runAsyncE . asyncBench n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runDeep . runForkE1 . forkBench n) op
  , bench "Fork (localUnlift/withLiftMap)" $
    nfAppIO (runDeep . runForkE2 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runDeep . runForkE3 . forkBench n) op
  , bench "Fork (localLiftUnlift)" $
    nfAppIO (runDeep . runForkE4 . forkBench n) op
  ]

op :: Monad m => m Int
op = pure 1

----------------------------------------

data ForkE :: Effect where
  ForkWithUnmask :: ((forall a. m a -> m a) -> m r) -> ForkE m (Async r)

-- | Uses 'localUnliftIO' and 'withLiftMapIO'.
runForkE1 :: IOE :> es => Eff (ForkE : es) a -> Eff es a
runForkE1 = interpret $ \env -> \case
  ForkWithUnmask m -> withLiftMapIO $ \liftMap -> do
    localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask

-- | Uses 'localUnlift' and 'withLiftMap'.
runForkE2 :: IOE :> es => Eff (ForkE : es) a -> Eff es a
runForkE2 = reinterpret A.runAsyncE $ \env -> \case
  ForkWithUnmask m -> withLiftMap $ \liftMap -> do
    localUnlift env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
      A.asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask

-- | Uses 'localLiftUnliftIO'.
runForkE3 :: IOE :> es => Eff (ForkE : es) a -> Eff es a
runForkE3 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnliftIO env (ConcUnlift Persistent $ Limited 1) $ \lift unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift

-- | Uses 'localLiftUnlift'.
runForkE4 :: IOE :> es => Eff (ForkE : es) a -> Eff es a
runForkE4 = reinterpret A.runAsyncE $ \env -> \case
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

asyncBench :: A.AsyncE :> es => Int -> Eff es Int -> Eff es Int
asyncBench n f = do
  a <- A.asyncWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  A.wait a
{-# NOINLINE asyncBench #-}

forkBench :: (IOE :> es, ForkE :> es) => Int -> Eff es Int -> Eff es Int
forkBench n f = do
  a <- send $ ForkWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  liftIO $ wait a
{-# NOINLINE forkBench #-}
