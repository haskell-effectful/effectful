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

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ map countdown [1000, 10000]
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
    [ bench "effectful (PS)"           $ nfAppIO countdownEffectfulStaticDeep n
    , bench "effectful (PD)"           $ nfAppIO countdownEffectfulDynPureDeep n
    , bench "effectful (MS)"           $ nfAppIO countdownEffectfulMVarDeep n
    , bench "effectful (MD)"           $ nfAppIO countdownEffectfulDynMVarDeep n
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
