{-# LANGUAGE CPP #-}
module Countdown where

import qualified Effectful as E
import qualified Effectful.Reader.Static as E
import qualified Effectful.State.Static.Local as EL

programEffectfulLocal :: EL.State Integer E.:> es => E.Eff es Integer
programEffectfulLocal = do
  n <- EL.get @Integer
  if n <= 0
    then pure n
    else do
      EL.put (n - 1)
      programEffectfulLocal
{-# NOINLINE programEffectfulLocal #-}

countdownEffectfulLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . EL.runState n
  . runR . runR . runR . runR . runR
  $ programEffectfulLocal
  where
    runR = E.runReader ()
