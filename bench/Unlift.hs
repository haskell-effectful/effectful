{-# LANGUAGE CPP #-}
module Unlift (unliftBenchmark) where

#ifdef VERSION_criterion
import Criterion
import Criterion.Main
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench
#endif

import UnliftIO

import Effectful
import Effectful.State.Dynamic
import Utils
import qualified Effectful.Async as A

unliftBenchmark :: Benchmark
unliftBenchmark = bgroup "unlift"
  [ bgroup "reference"
    [ bench "dummy" $ nfIO benchDummy
    , bench "async" $ nfIO benchAsync
    , bench "concurrently" $ nfIO benchConcurrently
    ]
  , bgroup "shallow"
    [ bgroup "dummy"
      [ bench "seq" $ nfAppIO (runShallow . withUnliftStrategy SeqUnlift) benchDummy
      , shallowConc "conc" 1 benchDummyE benchDummy
      ]
    , shallowConc "async" 1 benchAsyncE benchAsync
    , shallowConc "concurrently" 2 benchConcurrentlyE benchConcurrently
    ]
  , bgroup "deep"
    [ bgroup "dummy"
      [ bench "seq" $ nfAppIO (runDeep . withUnliftStrategy SeqUnlift) benchDummy
      , deepConc "conc" 1 benchDummyE benchDummy
      ]
    , deepConc "async" 1 benchAsyncE benchAsync
    , deepConc "concurrently" 2 benchConcurrentlyE benchConcurrently
    ]
  ]

----------------------------------------

shallowConc
  :: String
  -> Int
  -> Eff '[A.AsyncE, IOE] ()
  -> Eff '[IOE] ()
  -> Benchmark
shallowConc name n r f = bgroup name
  [ bench "reference" $ nfAppIO (runShallow . A.runAsyncE) r
  , bench "ephemeral/limited" $ nfAppIO
    (runShallow . withUnliftStrategy (ConcUnlift Ephemeral $ Limited n)) f
  , bench "ephemeral/unlimited" $ nfAppIO
    (runShallow . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)) f
  , bench "persistent/limited" $ nfAppIO
    (runShallow . withUnliftStrategy (ConcUnlift Persistent $ Limited n)) f
  , bench "persistent/unlimited" $ nfAppIO
    (runShallow . withUnliftStrategy (ConcUnlift Persistent Unlimited)) f
  ]

deepConc
  :: String
  -> Int
  -> Eff '[ A.AsyncE
          , State (), State (), State (), State (), State ()
          , State (), State (), State (), State (), State ()
          , IOE] ()
  -> Eff '[ State (), State (), State (), State (), State ()
          , State (), State (), State (), State (), State ()
          , IOE] ()
  -> Benchmark
deepConc name n r f = bgroup name
  [ bench "reference" $ nfAppIO (runDeep . A.runAsyncE) r
  , bench "ephemeral/limited" $ nfAppIO
    (runDeep . withUnliftStrategy (ConcUnlift Ephemeral $ Limited n)) f
  , bench "ephemeral/unlimited" $ nfAppIO
    (runDeep . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)) f
  , bench "persistent/limited" $ nfAppIO
    (runDeep . withUnliftStrategy (ConcUnlift Persistent $ Limited n)) f
  , bench "persistent/unlimited" $ nfAppIO
    (runDeep . withUnliftStrategy (ConcUnlift Persistent Unlimited)) f
  ]

----------------------------------------

benchDummy :: MonadUnliftIO m => m ()
benchDummy = withRunInIO $ \runIO -> runIO $ pure ()

benchAsync :: MonadUnliftIO m => m ()
benchAsync = withAsync (pure ()) wait

benchConcurrently :: MonadUnliftIO m => m ()
benchConcurrently = concurrently_ (pure ()) (pure ())

benchDummyE :: Eff es ()
benchDummyE = pure ()
{-# NOINLINE benchDummyE #-}

benchAsyncE :: A.AsyncE :> es => Eff es ()
benchAsyncE = A.withAsync (pure ()) A.wait
{-# NOINLINE benchAsyncE #-}

benchConcurrentlyE :: A.AsyncE :> es => Eff es ()
benchConcurrentlyE = A.concurrently_ (pure ()) (pure ())
{-# NOINLINE benchConcurrentlyE #-}
