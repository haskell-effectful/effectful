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
      , shallowConc "conc" 1 benchDummy
      ]
    , shallowConc "async" 1 benchAsync
    , shallowConc "concurrently" 2 benchConcurrently
    ]
  , bgroup "deep"
    [ bgroup "dummy"
      [ bench "seq" $ nfAppIO (runDeep . withUnliftStrategy SeqUnlift) benchDummy
      , deepConc "conc" 1 benchDummy
      ]
    , deepConc "async" 1 benchAsync
    , deepConc "concurrently" 2 benchConcurrently
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
