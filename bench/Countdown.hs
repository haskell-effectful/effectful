{-# LANGUAGE CPP #-}
module Countdown where

import Data.Functor.Identity

-- eff
#ifdef VERSION_eff
import qualified Control.Effect as L
#endif

-- effective
import qualified Effective as E
import qualified Effective.Reader as E
import qualified Effective.State as E
import qualified Effective.State.MVar as ME
import qualified Effective.State.Dynamic as DE

-- freer-simple
#ifdef VERSION_freer_simple
import qualified Control.Monad.Freer as FS
import qualified Control.Monad.Freer.Reader as FS
import qualified Control.Monad.Freer.State as FS
#endif

-- fused-effects
#ifdef VERSION_fused_effects
import qualified Control.Algebra as FE
import qualified Control.Carrier.Reader as FE
import qualified Control.Carrier.State.Strict as FE
#endif

-- mtl
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M

-- polysemy
#ifdef VERSION_polysemy
import qualified Polysemy as P
import qualified Polysemy.Reader as P
import qualified Polysemy.State as P
#endif

----------------------------------------
-- reference

countdownRef :: Integer -> (Integer, Integer)
countdownRef n = if n <= 0 then (n, n) else countdownRef $ n - 1
{-# NOINLINE countdownRef #-}

----------------------------------------
-- mtl

programMtl :: M.MonadState Integer m => m Integer
programMtl = do
  n <- M.get @Integer
  if n <= 0
    then pure n
    else do
      M.put (n - 1)
      programMtl
{-# NOINLINE programMtl #-}

countdownMtl :: Integer -> (Integer, Integer)
countdownMtl n = flip M.runState n $ programMtl

countdownMtlDeep :: Integer -> (Integer, Integer)
countdownMtlDeep n = runIdentity
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . flip M.runStateT n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programMtl
  where
    runR = flip M.runReaderT ()

----------------------------------------
-- effective (pure)

programEffectiveStatic :: E.State Integer E.:> es => E.Eff es Integer
programEffectiveStatic = do
  n <- E.get @Integer
  if n <= 0
    then pure n
    else do
      E.put (n - 1)
      programEffectiveStatic
{-# NOINLINE programEffectiveStatic #-}

countdownEffectiveStatic :: Integer -> IO (Integer, Integer)
countdownEffectiveStatic n = E.runEff . E.runState n $ programEffectiveStatic

countdownEffectiveStaticDeep :: Integer -> IO (Integer, Integer)
countdownEffectiveStaticDeep n = E.runEff
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . E.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programEffectiveStatic
  where
    runR = E.runReader ()

----------------------------------------
-- effective (mvar)

programEffectiveMVar :: ME.State Integer E.:> es => E.Eff es Integer
programEffectiveMVar = do
  n <- ME.get @Integer
  if n <= 0
    then pure n
    else do
      ME.put (n - 1)
      programEffectiveMVar
{-# NOINLINE programEffectiveMVar #-}

countdownEffectiveMVar :: Integer -> IO (Integer, Integer)
countdownEffectiveMVar n = E.runEff . ME.runState n $ programEffectiveMVar

countdownEffectiveMVarDeep :: Integer -> IO (Integer, Integer)
countdownEffectiveMVarDeep n = E.runEff
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . ME.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programEffectiveMVar
  where
    runR = E.runReader ()

----------------------------------------
-- effective (dynamic)

programEffectiveDynamic :: DE.State Integer E.:> es => E.Eff es Integer
programEffectiveDynamic = do
  n <- DE.get @Integer
  if n <= 0
    then pure n
    else do
      DE.put (n - 1)
      programEffectiveDynamic
{-# NOINLINE programEffectiveDynamic #-}

countdownEffectiveDynPure :: Integer -> IO (Integer, Integer)
countdownEffectiveDynPure n = E.runEff . DE.runState n $ programEffectiveDynamic

countdownEffectiveDynMVar :: Integer -> IO (Integer, Integer)
countdownEffectiveDynMVar n = E.runEff . DE.runStateMVar n $ programEffectiveDynamic

countdownEffectiveDynPureDeep :: Integer -> IO (Integer, Integer)
countdownEffectiveDynPureDeep n = E.runEff
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . DE.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programEffectiveDynamic
  where
    runR = E.runReader ()

countdownEffectiveDynMVarDeep :: Integer -> IO (Integer, Integer)
countdownEffectiveDynMVarDeep n = E.runEff
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . DE.runStateMVar n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programEffectiveDynamic
  where
    runR = E.runReader ()

----------------------------------------
-- fused-effects

#ifdef VERSION_fused_effects

programFusedEffects :: FE.Has (FE.State Integer) sig m => m Integer
programFusedEffects = do
  n <- FE.get @Integer
  if n <= 0
    then pure n
    else do
      FE.put (n - 1)
      programFusedEffects
{-# NOINLINE programFusedEffects #-}

countdownFusedEffects :: Integer -> (Integer, Integer)
countdownFusedEffects n = FE.run . FE.runState n $ programFusedEffects

countdownFusedEffectsDeep :: Integer -> (Integer, Integer)
countdownFusedEffectsDeep n = FE.run
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . FE.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programFusedEffects
  where
    runR = FE.runReader ()

#endif

----------------------------------------
-- polysemy

#ifdef VERSION_polysemy

programPolysemy :: P.Member (P.State Integer) r => P.Sem r Integer
programPolysemy = do
  n <- P.get @Integer
  if n <= 0
    then pure n
    else do
      P.put (n - 1)
      programPolysemy
{-# NOINLINE programPolysemy #-}

countdownPolysemy :: Integer -> (Integer, Integer)
countdownPolysemy n = P.run . P.runState n $ programPolysemy

countdownPolysemyDeep :: Integer -> (Integer, Integer)
countdownPolysemyDeep n = P.run
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . P.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programPolysemy
  where
    runR = P.runReader ()

#endif

----------------------------------------
-- eff

#ifdef VERSION_eff

programEff :: L.State Integer L.:< es => L.Eff es Integer
programEff = do
  n <- L.get @Integer
  if n <= 0
    then pure n
    else do
      L.put (n - 1)
      programEff
{-# NOINLINE programEff #-}

countdownEff :: Integer -> (Integer, Integer)
countdownEff n = L.run . L.runState n $ programEff

countdownEffDeep :: Integer -> (Integer, Integer)
countdownEffDeep n = L.run
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . L.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programEff
  where
    runR = L.runReader ()

#endif

----------------------------------------
-- freer-simple

#ifdef VERSION_freer_simple

programFreerSimple :: FS.Member (FS.State Integer) es => FS.Eff es Integer
programFreerSimple = do
  n <- FS.get @Integer
  if n <= 0
    then pure n
    else do
      FS.put (n - 1)
      programFreerSimple
{-# NOINLINE programFreerSimple #-}

countdownFreerSimple :: Integer -> (Integer, Integer)
countdownFreerSimple n = FS.run . FS.runState n $ programFreerSimple

countdownFreerSimpleDeep :: Integer -> (Integer, Integer)
countdownFreerSimpleDeep n = FS.run
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  . FS.runState n
  . runR . runR . runR . runR . runR . runR . runR . runR . runR . runR
  $ programFreerSimple
  where
    runR = FS.runReader ()

#endif
