{-# LANGUAGE CPP #-}
module Concurrency (concurrencyBenchmark) where

#ifdef VERSION_criterion
import Criterion hiding (env)
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench hiding (env)
#endif

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

import Effectful
import Effectful.Concurrent.Async qualified as A
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Utils

concurrencyBenchmark :: Benchmark
concurrencyBenchmark = bgroup "concurrency"
  [ bgroup "shallow" $
    [ bench "MultiFork" $ nfAppIO (runShallow . testMultiFork) 1000
    ] ++ map shallowBenchUnmask [1, 10, 100]
  , bgroup "deep" $
    [ bench "MultiFork" $ nfAppIO (runDeep . testMultiFork) 1000
    ] ++ map deepBenchUnmask [1, 10, 100]
  ]

testMultiFork :: IOE :> es => Int -> Eff es ()
testMultiFork threads = runConcurrent $ do
  v <- newTVarIO 0
  withEffToIO (ConcUnlift Persistent $ Limited threads) $ \unlift -> do
    replicateM_ threads . liftIO . forkIO . unlift . atomically $ modifyTVar' v (+1)
  atomically $ do
    acc <- readTVar v
    when (acc < threads) retry

----------------------------------------

shallowBenchUnmask :: Int -> Benchmark
shallowBenchUnmask n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (IO)" $ nfAppIO (asyncBenchIO n) op
  , bench "async (Eff)" $ nfAppIO (runShallow . A.runConcurrent . asyncBench n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runShallow . runFork1 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runShallow . runFork2 . forkBench n) op
  , bench "Fork (localLiftUnlift)" $
    nfAppIO (runShallow . runFork3 . forkBench n) op
  ]

deepBenchUnmask :: Int -> Benchmark
deepBenchUnmask n = bgroup ("unmask " ++ show n ++ "x")
  [ bench "async (Eff)" $ nfAppIO (runDeep . A.runConcurrent . asyncBench n) op
  , bench "Fork (localUnliftIO/withLiftMapIO)" $
    nfAppIO (runDeep . runFork1 . forkBench n) op
  , bench "Fork (localLiftUnliftIO)" $
    nfAppIO (runDeep . runFork2 . forkBench n) op
  , bench "Fork (localLiftUnlift)" $
    nfAppIO (runDeep . runFork3 . forkBench n) op
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

-- | Uses 'localLiftUnliftIO'.
runFork2 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork2 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnliftIO env (ConcUnlift Persistent $ Limited 1) $ \lift unlift -> do
      asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift

-- | Uses 'localLiftUnlift'.
runFork3 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork3 = reinterpret A.runConcurrent $ \env -> \case
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
