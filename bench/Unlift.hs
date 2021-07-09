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

unliftBenchmark :: Benchmark
unliftBenchmark = bgroup "unlift"
  [ bgroup "reference"
    [ bench "dummy" $ nfIO benchDummy
    , bench "async" $ nfIO benchAsync
    , bench "concurrently" $ nfIO benchConcurrently
    ]
  , bgroup "shallow"
    [ bgroup "dummy"
      [ bench "seq" $ nfAppIO (runShallow SeqUnlift) benchDummy
      , shallowConc "conc" 1 benchDummy
      ]
    , shallowConc "async" 1 benchAsync
    , shallowConc "concurrently" 2 benchConcurrently
    ]
  , bgroup "deep"
    [ bgroup "dummy"
      [ bench "seq" $ nfAppIO (runDeep SeqUnlift) benchDummy
      , deepConc "conc" 1 benchDummy
      ]
    , deepConc "async" 1 benchAsync
    , deepConc "concurrently" 2 benchConcurrently
    ]
  ]

----------------------------------------

runShallow :: UnliftStrategy -> Eff '[IOE] a -> IO a
runShallow s = runIOE . withUnliftStrategy s

runDeep
  :: UnliftStrategy
  -> Eff '[ State (), State (), State (), State (), State ()
          , State (), State (), State (), State (), State ()
          , IOE
          ] a
  -> IO a
runDeep s = runIOE
  . evalState () . evalState () . evalState () . evalState () . evalState ()
  . evalState () . evalState () . evalState () . evalState () . evalState ()
  . withUnliftStrategy s

----------------------------------------

shallowConc
  :: String
  -> Int
  -> Eff '[IOE] ()
  -> Benchmark
shallowConc name n f = bgroup name
  [ bench "ephemeral/limited" $ nfAppIO
    (runShallow (ConcUnlift Ephemeral $ Limited n)) f
  , bench "ephemeral/unlimited" $ nfAppIO
    (runShallow (ConcUnlift Ephemeral Unlimited)) f
  , bench "persistent/limited" $ nfAppIO
    (runShallow (ConcUnlift Persistent $ Limited n)) f
  , bench "persistent/unlimited" $ nfAppIO
    (runShallow (ConcUnlift Persistent Unlimited)) f
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
    (runDeep (ConcUnlift Ephemeral $ Limited n)) f
  , bench "ephemeral/unlimited" $ nfAppIO
    (runDeep (ConcUnlift Ephemeral Unlimited)) f
  , bench "persistent/limited" $ nfAppIO
    (runDeep (ConcUnlift Persistent $ Limited n)) f
  , bench "persistent/unlimited" $ nfAppIO
    (runDeep (ConcUnlift Persistent Unlimited)) f
  ]

----------------------------------------

benchDummy :: MonadUnliftIO m => m ()
benchDummy = withRunInIO $ \runIO -> runIO $ pure ()

benchAsync :: MonadUnliftIO m => m ()
benchAsync = withAsync (pure ()) wait

benchConcurrently :: MonadUnliftIO m => m ()
benchConcurrently = concurrently_ (pure ()) (pure ())
