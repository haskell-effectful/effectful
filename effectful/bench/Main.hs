{-# LANGUAGE CPP #-}
module Main (main) where

#ifdef VERSION_criterion
import Criterion
import Criterion.Main
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench
#endif

import Concurrency
import Countdown
import FileSizes
import Unlift

main :: IO ()
main = defaultMain
  [ concurrencyBenchmark
  , unliftBenchmark
  , bgroup "countdown" $ map countdown [1000, 10000]
  , bgroup "filesize"  $ map filesize [100, 1000]
  ]

countdown :: Integer -> Benchmark
countdown n = bgroup (show n)
  [ bench "reference (pure)"             $ nf countdownRef n
  , bench "reference (ST)"               $ nf countdownST n
  , bgroup "shallow"
    [ bench "effectful (local/static)"   $ nf countdownEffectfulLocal n
    , bench "effectful (local/dynamic)"  $ nf countdownEffectfulDynLocal n
    , bench "effectful (shared/static)"  $ nf countdownEffectfulShared n
    , bench "effectful (shared/dynamic)" $ nf countdownEffectfulDynShared n
#ifdef VERSION_freer_simple
    , bench "freer-simple"               $ nf countdownFreerSimple n
#endif
#ifdef VERSION_eff
    , bench "eff"                        $ nf countdownEff n
#endif
#ifdef VERSION_mtl
    , bench "mtl"                        $ nf countdownMtl n
#endif
#ifdef VERSION_fused_effects
    , bench "fused-effects"              $ nf countdownFusedEffects n
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"                   $ nf countdownPolysemy n
#endif
    ]
  , bgroup "deep"
    [ bench "effectful (local/static)"   $ nf countdownEffectfulLocalDeep n
    , bench "effectful (local/dynamic)"  $ nf countdownEffectfulDynLocalDeep n
    , bench "effectful (shared/static)"  $ nf countdownEffectfulSharedDeep n
    , bench "effectful (shared/dynamic)" $ nf countdownEffectfulDynSharedDeep n
#ifdef VERSION_freer_simple
    , bench "freer-simple"               $ nf countdownFreerSimpleDeep n
#endif
#ifdef VERSION_eff
    , bench "eff"                        $ nf countdownEffDeep n
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"                   $ nf countdownPolysemyDeep n
#endif
#ifdef VERSION_mtl
    , bench "mtl"                        $ nf countdownMtlDeep n
#endif
#ifdef VERSION_fused_effects
    , bench "fused-effects"              $ nf countdownFusedEffectsDeep n
#endif
    ]
  ]

filesize :: Int -> Benchmark
filesize n = bgroup (show n)
  [ bench "reference"       $ nfAppIO ref_calculateFileSizes (take n files)
  , bgroup "shallow"
    [ bench "effectful"     $ nfAppIO effectful_calculateFileSizes (take n files)
#ifdef VERSION_freer_simple
    , bench "freer-simple"  $ nfAppIO fs_calculateFileSizes (take n files)
#endif
#ifdef VERSION_mtl
    , bench "mtl"           $ nfAppIO mtl_calculateFileSizes (take n files)
#endif
#ifdef VERSION_eff
    , bench "eff"           $ nfAppIO eff_calculateFileSizes (take n files)
#endif
#ifdef VERSION_fused_effects
    , bench "fused-effects" $ nfAppIO fe_calculateFileSizes (take n files)
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"      $ nfAppIO poly_calculateFileSizes (take n files)
#endif
    ]
  , bgroup "deep"
    [ bench "effectful"     $ nfAppIO effectful_calculateFileSizesDeep (take n files)
#ifdef VERSION_freer_simple
    , bench "freer-simple"  $ nfAppIO fs_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_eff
    , bench "eff"           $ nfAppIO eff_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_mtl
    , bench "mtl"           $ nfAppIO mtl_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"      $ nfAppIO poly_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_fused_effects
    , bench "fused-effects" $ nfAppIO fe_calculateFileSizesDeep (take n files)
#endif
    ]
  ]
  where
    files :: [FilePath]
    files = repeat "effectful.cabal"
