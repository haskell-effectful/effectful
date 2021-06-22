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

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ map countdown [1000, 10000]
  , bgroup "filesize"  $ map filesize [100, 1000]
  ]

countdown :: Integer -> Benchmark
countdown n = bgroup (show n)
  [ bgroup "shallow"
    [ bench "reference"                $ nf countdownRef n
    , bench "effectful (pure-static)"  $ nfAppIO countdownEffectfulStatic n
    , bench "effectful (pure-dynamic)" $ nfAppIO countdownEffectfulDynPure n
    , bench "effectful (MVar-static)"  $ nfAppIO countdownEffectfulMVar n
    , bench "effectful (MVar-dynamic)" $ nfAppIO countdownEffectfulDynMVar n
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
    [ bench "effectful (pure-static)"  $ nfAppIO countdownEffectfulStaticDeep n
    , bench "effectful (pure-dynamic)" $ nfAppIO countdownEffectfulDynPureDeep n
    , bench "effectful (MVar-static)"  $ nfAppIO countdownEffectfulMVarDeep n
    , bench "effectful (MVar-dynamic)" $ nfAppIO countdownEffectfulDynMVarDeep n
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
