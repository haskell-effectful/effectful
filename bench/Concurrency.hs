{-# LANGUAGE CPP #-}
module Concurrency (concurrencyBenchmark) where

#ifdef VERSION_criterion
import Criterion
import Criterion.Main
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
  [ bench "async (IO)" $ nfAppIO asyncBenchIO (pure 1)
  , bgroup "shallow"
    [ bench "async (Eff)" $ nfAppIO (runShallow . A.runAsyncE . asyncBench) (pure 1)
    , bench "Fork (1)" $ nfAppIO (runShallow . runFork1 . forkBench) (pure 1)
    , bench "Fork (2)" $ nfAppIO (runShallow . runFork2 . forkBench) (pure 1)
    ]
  , bgroup "deep"
    [ bench "async (Eff)" $ nfAppIO (runDeep . A.runAsyncE . asyncBench) (pure 1)
    , bench "Fork (1)" $ nfAppIO (runDeep . runFork1 . forkBench) (pure 1)
    , bench "Fork (2)" $ nfAppIO (runDeep . runFork2 . forkBench) (pure 1)
    ]
  ]

----------------------------------------

data Fork :: Effect where
  ForkWithUnmask :: ((forall a. m a -> m a) -> m r) -> Fork m (Async r)

-- | Uses 'localUnliftIO' and 'withLiftSeqOp'.
runFork1 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork1 = interpretM $ \env (ForkWithUnmask m) -> withLiftSeqOp $ \liftSeqOp -> do
  localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
    asyncWithUnmask $ \unmask -> unlift $ m $ liftSeqOp unmask

-- | Uses 'localLiftUnliftIO', slower than (1).
runFork2 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork2 = interpretM $ \env (ForkWithUnmask m) -> do
  localLiftUnliftIO env (ConcUnlift Persistent $ Limited 1) $ \lift unlift -> do
    asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift

----------------------------------------

n :: Int
n = 100

asyncBenchIO :: IO Int -> IO Int
asyncBenchIO f = do
  a <- asyncWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  wait a
{-# NOINLINE asyncBenchIO #-}

asyncBench :: A.AsyncE :> es => Eff es Int -> Eff es Int
asyncBench f = do
  a <- A.asyncWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  A.wait a
{-# NOINLINE asyncBench #-}

forkBench :: (IOE :> es, Fork :> es) => Eff es Int -> Eff es Int
forkBench f = do
  a <- send $ ForkWithUnmask $ \unmask -> do
    sum <$> replicateM n (unmask f)
  liftIO $ wait a
{-# NOINLINE forkBench #-}
