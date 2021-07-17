{-# LANGUAGE CPP #-}
module Unlift (unliftBenchmark) where

#ifdef VERSION_criterion
import Criterion
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench
#endif

import UnliftIO

import Effectful.Monad
import Effectful.State.Dynamic
import Utils

unliftBenchmark :: Benchmark
unliftBenchmark = bgroup "unlifting"
  [ bgroup "reference"
    [ bench "dummy" $ nfIO benchDummy
    , bench "async" $ nfIO benchAsync
    , bench "concurrently" $ nfIO benchConcurrently
    ]
  , bgroup "shallow"
    [ bgroup "dummy"
      [ bench "noop" $ nfAppIO runShallow benchDummyE
      , bench "seq" $ nfAppIO runShallow benchDummy
      , shallowConc "conc" 1 benchDummy
      ]
    ]
  , bgroup "deep"
    [ bgroup "dummy"
      [ bench "noop" $ nfAppIO runDeep benchDummyE
      , bench "seq" $ nfAppIO runDeep benchDummy
      , deepConc "conc" 1 benchDummy
      ]
    ]
  ]

----------------------------------------

shallowConc
  :: String
  -> Int
  -> Eff '[IOE] ()
  -> Benchmark
shallowConc name n f = bgroup name
  [ bench "ephemeral/limited" $ nfAppIO
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
  -> Eff '[ State (), State (), State (), State (), State ()
          , State (), State (), State (), State (), State ()
          , IOE] ()
  -> Benchmark
deepConc name n f = bgroup name
  [ bench "ephemeral/limited" $ nfAppIO
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
