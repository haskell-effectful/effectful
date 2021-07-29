{-# LANGUAGE CPP #-}
module Countdown where

import Control.Monad.ST
import Data.STRef

-- eff
#ifdef VERSION_eff
import qualified Control.Effect as L
#endif

-- effectful
import qualified Effectful as E
import qualified Effectful.Reader as E
import qualified Effectful.State.Dynamic as ED
import qualified Effectful.State.Local as EL
import qualified Effectful.State.Shared as ES

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
#ifdef VERSION_mtl
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import Data.Functor.Identity
#endif

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
-- ST

programST :: STRef s Integer -> ST s Integer
programST ref = do
  n <- readSTRef ref
  if n <= 0
    then pure n
    else do
      writeSTRef ref $! n - 1
      programST ref
{-# NOINLINE programST #-}

countdownST :: Integer -> (Integer, Integer)
countdownST n = runST $ do
  ref <- newSTRef n
  a <- programST ref
  s <- readSTRef ref
  pure (a, s)

----------------------------------------
-- mtl

#ifdef VERSION_mtl

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
  . runR . runR . runR . runR . runR
  . flip M.runStateT n
  . runR . runR . runR . runR . runR
  $ programMtl
  where
    runR = flip M.runReaderT ()

#endif

----------------------------------------
-- effectful (pure)

programEffectfulLocal :: EL.StateE Integer E.:> es => E.Eff es Integer
programEffectfulLocal = do
  n <- EL.get @Integer
  if n <= 0
    then pure n
    else do
      EL.put (n - 1)
      programEffectfulLocal
{-# NOINLINE programEffectfulLocal #-}

countdownEffectfulLocal :: Integer -> (Integer, Integer)
countdownEffectfulLocal n = E.runPureEff . EL.runStateE n $ programEffectfulLocal

countdownEffectfulLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . EL.runStateE n
  . runR . runR . runR . runR . runR
  $ programEffectfulLocal
  where
    runR = E.runReaderE ()

----------------------------------------
-- effectful (mvar)

programEffectfulShared :: ES.StateE Integer E.:> es => E.Eff es Integer
programEffectfulShared = do
  n <- ES.get @Integer
  if n <= 0
    then pure n
    else do
      ES.put (n - 1)
      programEffectfulShared
{-# NOINLINE programEffectfulShared #-}

countdownEffectfulShared :: Integer -> (Integer, Integer)
countdownEffectfulShared n = E.runPureEff . ES.runStateE n $ programEffectfulShared

countdownEffectfulSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ES.runStateE n
  . runR . runR . runR . runR . runR
  $ programEffectfulShared
  where
    runR = E.runReaderE ()

----------------------------------------
-- effectful (dynamic)

programEffectfulDynamic :: ED.StateE Integer E.:> es => E.Eff es Integer
programEffectfulDynamic = do
  n <- ED.get @Integer
  if n <= 0
    then pure n
    else do
      ED.put (n - 1)
      programEffectfulDynamic
{-# NOINLINE programEffectfulDynamic #-}

countdownEffectfulDynLocal :: Integer -> (Integer, Integer)
countdownEffectfulDynLocal n =
  E.runPureEff . ED.runLocalStateE n $ programEffectfulDynamic

countdownEffectfulDynShared :: Integer -> (Integer, Integer)
countdownEffectfulDynShared n =
  E.runPureEff . ED.runSharedStateE n $ programEffectfulDynamic

countdownEffectfulDynLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulDynLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ED.runLocalStateE n
  . runR . runR . runR . runR . runR
  $ programEffectfulDynamic
  where
    runR = E.runReaderE ()

countdownEffectfulDynSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulDynSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ED.runSharedStateE n
  . runR . runR . runR . runR . runR
  $ programEffectfulDynamic
  where
    runR = E.runReaderE ()

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
  . runR . runR . runR . runR . runR
  . FE.runState n
  . runR . runR . runR . runR . runR
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
  . runR . runR . runR . runR . runR
  . P.runState n
  . runR . runR . runR . runR . runR
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
  . runR . runR . runR . runR . runR
  . L.runState n
  . runR . runR . runR . runR . runR
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
  . runR . runR . runR . runR . runR
  . FS.runState n
  . runR . runR . runR . runR . runR
  $ programFreerSimple
  where
    runR = FS.runReader ()

#endif
