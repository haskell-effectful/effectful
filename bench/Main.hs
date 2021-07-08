{-# LANGUAGE CPP #-}
module Main (main) where

#ifdef VERSION_criterion
import Criterion
import Criterion.Main
#endif

#ifdef VERSION_tasty_bench
import Test.Tasty.Bench
#endif

import Countdown
import FileSizes
import Unlift

main :: IO ()
main = defaultMain
  [ unliftBenchmark
  , bgroup "countdown" $ map countdown [1000, 10000]
  , bgroup "filesize"  $ map filesize [100, 1000]
  ]

countdown :: Integer -> Benchmark
countdown n = bgroup (show n)
  [ bgroup "shallow"
    [ bench "reference (pure)"         $ nf countdownRef n
    , bench "reference (ST)"           $ nf countdownST n
    , bench "effectful (pure-static)"  $ nf countdownEffectfulStatic n
    , bench "effectful (pure-dynamic)" $ nf countdownEffectfulDynPure n
    , bench "effectful (MVar-static)"  $ nf countdownEffectfulMVar n
    , bench "effectful (MVar-dynamic)" $ nf countdownEffectfulDynMVar n
#ifdef VERSION_freer_simple
    , bench "freer-simple"             $ nf countdownFreerSimple n
#endif
#ifdef VERSION_eff
    , bench "eff"                      $ nf countdownEff n
#endif
    , bench "mtl"                      $ nf countdownMtl n
#ifdef VERSION_fused_effects
    , bench "fused-effects"            $ nf countdownFusedEffects n
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"                 $ nf countdownPolysemy n
#endif
    ]
  , bgroup "deep"
    [ bench "effectful (pure-static)"  $ nf countdownEffectfulStaticDeep n
    , bench "effectful (pure-dynamic)" $ nf countdownEffectfulDynPureDeep n
    , bench "effectful (MVar-static)"  $ nf countdownEffectfulMVarDeep n
    , bench "effectful (MVar-dynamic)" $ nf countdownEffectfulDynMVarDeep n
#ifdef VERSION_freer_simple
    , bench "freer-simple"             $ nf countdownFreerSimpleDeep n
#endif
#ifdef VERSION_eff
    , bench "eff"                      $ nf countdownEffDeep n
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"                 $ nf countdownPolysemyDeep n
#endif
    , bench "mtl"                      $ nf countdownMtlDeep n
#ifdef VERSION_fused_effects
    , bench "fused-effects"            $ nf countdownFusedEffectsDeep n
#endif
    ]
  ]

filesize :: Int -> Benchmark
filesize n = bgroup (show n)
  [ bgroup "shallow"
    [ bench "reference"    $ nfAppIO ref_calculateFileSizes (take n files)
    , bench "effectful"    $ nfAppIO effectful_calculateFileSizes (take n files)
#ifdef VERSION_freer_simple
    , bench "freer-simple" $ nfAppIO fs_calculateFileSizes (take n files)
#endif
    , bench "mtl"          $ nfAppIO mtl_calculateFileSizes (take n files)
#ifdef VERSION_eff
    , bench "eff"          $ nfAppIO eff_calculateFileSizes (take n files)
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"     $ nfAppIO poly_calculateFileSizes (take n files)
#endif
    ]
  , bgroup "deep"
    [ bench "effectful"    $ nfAppIO effectful_calculateFileSizesDeep (take n files)
#ifdef VERSION_freer_simple
    , bench "freer-simple" $ nfAppIO fs_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_eff
    , bench "eff"          $ nfAppIO eff_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_polysemy
    , bench "polysemy"     $ nfAppIO poly_calculateFileSizesDeep (take n files)
#endif
    , bench "mtl"          $ nfAppIO mtl_calculateFileSizesDeep (take n files)
    ]
  ]
  where
    files :: [FilePath]
    files = repeat "effectful.cabal"
