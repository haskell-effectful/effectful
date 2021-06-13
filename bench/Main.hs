{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty.Bench

import Countdown

main :: IO ()
main = defaultMain
  [ bgroup "countdown"
    [ bgroup "reference"        $ mkCountdown nf      countdownRef
    , bgroup "shallow"
      [ bgroup "efectful (SP)"  $ mkCountdown nfAppIO countdownEffectfulStatic
      , bgroup "efectful (SM)"  $ mkCountdown nfAppIO countdownEffectfulMVar
      , bgroup "efectful (DP)"  $ mkCountdown nfAppIO countdownEffectfulDynPure
      , bgroup "efectful (DM)"  $ mkCountdown nfAppIO countdownEffectfulDynMVar
#ifdef VERSION_freer_simple
      , bgroup "freer-simple"   $ mkCountdown nf      countdownFreerSimple
#endif
#ifdef VERSION_eff
      , bgroup "eff"            $ mkCountdown nf      countdownEff
#endif
      , bgroup "mtl"            $ mkCountdown nf      countdownMtl
#ifdef VERSION_fused_effects
      , bgroup "fused-effects"  $ mkCountdown nf      countdownFusedEffects
#endif
#ifdef VERSION_polysemy
      , bgroup "polysemy"       $ mkCountdown nf      countdownPolysemy
#endif
      ]
    , bgroup "deep"
      [ bgroup "efectful (SP)"  $ mkCountdown nfAppIO countdownEffectfulStaticDeep
      , bgroup "efectful (SM)"  $ mkCountdown nfAppIO countdownEffectfulMVarDeep
      , bgroup "efectful (DP)"  $ mkCountdown nfAppIO countdownEffectfulDynPureDeep
      , bgroup "efectful (DM)"  $ mkCountdown nfAppIO countdownEffectfulDynMVarDeep
#ifdef VERSION_eff
      , bgroup "eff"            $ mkCountdown nf      countdownEffDeep
#endif
#ifdef VERSION_freer_simple
      , bgroup "freer-simple"   $ mkCountdown nf      countdownFreerSimpleDeep
#endif
#ifdef VERSION_polysemy
      , bgroup "polysemy"       $ mkCountdown nf      countdownPolysemyDeep
#endif
      , bgroup "mtl"            $ mkCountdown nf      countdownMtlDeep
#ifdef VERSION_fused_effects
      , bgroup "fused-effects"  $ mkCountdown nf      countdownFusedEffectsDeep
#endif
      ]
    ]
  ]

mkCountdown :: (nf -> Integer -> Benchmarkable) -> nf -> [Benchmark]
mkCountdown f g = map (\n -> bench (show n) $ f g n) [100, 1000, 10000]
