{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty.Bench

import Countdown

main :: IO ()
main = defaultMain
  [ bgroup "countdown"
    [ bgroup "reference"       $ mkCountdown nf      countdownRef
    , bgroup "shallow"
      [ bgroup "effective (S)" $ mkCountdown nfAppIO countdownEffectiveStatic
      , bgroup "effective (D)" $ mkCountdown nfAppIO countdownEffectiveDynamic
#ifdef VERSION_freer_simple
      , bgroup "freer-simple"  $ mkCountdown nf      countdownFreerSimple
#endif
#ifdef VERSION_eff
      , bgroup "eff"           $ mkCountdown nf      countdownEff
#endif
      , bgroup "mtl"           $ mkCountdown nf      countdownMtl
#ifdef VERSION_fused_effects
      , bgroup "fused-effects" $ mkCountdown nf      countdownFusedEffects
#endif
#ifdef VERSION_polysemy
      , bgroup "polysemy"      $ mkCountdown nf      countdownPolysemy
#endif
      ]
    , bgroup "deep"
      [ bgroup "effective (S)" $ mkCountdown nfAppIO countdownEffectiveStaticDeep
      , bgroup "effective (D)" $ mkCountdown nfAppIO countdownEffectiveDynamicDeep
#ifdef VERSION_eff
      , bgroup "eff"           $ mkCountdown nf      countdownEffDeep
#endif
#ifdef VERSION_freer_simple
      , bgroup "freer-simple"  $ mkCountdown nf      countdownFreerSimpleDeep
#endif
#ifdef VERSION_polysemy
      , bgroup "polysemy"      $ mkCountdown nf      countdownPolysemyDeep
#endif
      , bgroup "mtl"           $ mkCountdown nf      countdownMtlDeep
#ifdef VERSION_fused_effects
      , bgroup "fused-effects" $ mkCountdown nf      countdownFusedEffectsDeep
#endif
      ]
    ]
  ]

mkCountdown :: (nf -> Integer -> Benchmarkable) -> nf -> [Benchmark]
mkCountdown f g = map (\n -> bench (show n) $ f g n) [100, 1000, 10000]
