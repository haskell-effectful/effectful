{-# LANGUAGE CPP #-}
module Countdown where

import Control.Monad.ST
import Data.STRef

-- eff
#ifdef VERSION_eff
import Control.Effect qualified as L
#endif

-- cleff
#ifdef VERSION_cleff
import Cleff qualified as C
import Cleff.Reader qualified as C
import Cleff.State qualified as C
#endif

-- effectful
import Effectful qualified as E
import Effectful.Dispatch.Dynamic qualified as E
import Effectful.Labeled qualified as E
import Effectful.Reader.Static qualified as E
import Effectful.State.Dynamic qualified as ED
import Effectful.State.Static.Local qualified as EL
import Effectful.State.Static.Shared qualified as ES

-- freer-simple
#ifdef VERSION_freer_simple
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Freer.State qualified as FS
#endif

-- fused-effects
#ifdef VERSION_fused_effects
import Control.Algebra qualified as FE
import Control.Carrier.Reader qualified as FE
import Control.Carrier.State.Strict qualified as FE
#endif

-- mtl
#ifdef VERSION_mtl
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Data.Functor.Identity
#endif

-- polysemy
#ifdef VERSION_polysemy
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import Polysemy.State qualified as P
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

programEffectfulLocal :: EL.State Integer E.:> es => E.Eff es Integer
programEffectfulLocal = do
  n <- EL.get @Integer
  if n <= 0
    then pure n
    else do
      EL.put (n - 1)
      programEffectfulLocal
{-# NOINLINE programEffectfulLocal #-}

countdownEffectfulLocal :: Integer -> (Integer, Integer)
countdownEffectfulLocal n = E.runPureEff . EL.runState n $ programEffectfulLocal

countdownEffectfulLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . EL.runState n
  . runR . runR . runR . runR . runR
  $ programEffectfulLocal
  where
    runR = E.runReader ()

----

programEffectfulLocalSt :: EL.State Integer E.:> es => E.Eff es Integer
programEffectfulLocalSt = do
  n <- EL.state @Integer $ \s -> (s, s - 1)
  if n <= 0
    then pure n
    else programEffectfulLocalSt
{-# NOINLINE programEffectfulLocalSt #-}

countdownEffectfulLocalSt :: Integer -> (Integer, Integer)
countdownEffectfulLocalSt n = E.runPureEff . EL.runState n $ programEffectfulLocalSt

countdownEffectfulLocalDeepSt :: Integer -> (Integer, Integer)
countdownEffectfulLocalDeepSt n = E.runPureEff
  . runR . runR . runR . runR . runR
  . EL.runState n
  . runR . runR . runR . runR . runR
  $ programEffectfulLocalSt
  where
    runR = E.runReader ()

----

programEffectfulLocalStM :: EL.State Integer E.:> es => E.Eff es Integer
programEffectfulLocalStM = do
  n <- EL.stateM @Integer $ \s -> pure (s, s - 1)
  if n <= 0
    then pure n
    else programEffectfulLocalStM
{-# NOINLINE programEffectfulLocalStM #-}

countdownEffectfulLocalStM :: Integer -> (Integer, Integer)
countdownEffectfulLocalStM n = E.runPureEff . EL.runState n $ programEffectfulLocalStM

countdownEffectfulLocalDeepStM :: Integer -> (Integer, Integer)
countdownEffectfulLocalDeepStM n = E.runPureEff
  . runR . runR . runR . runR . runR
  . EL.runState n
  . runR . runR . runR . runR . runR
  $ programEffectfulLocalStM
  where
    runR = E.runReader ()

----------------------------------------
-- effectful (mvar)

programEffectfulShared :: ES.State Integer E.:> es => E.Eff es Integer
programEffectfulShared = do
  n <- ES.get @Integer
  if n <= 0
    then pure n
    else do
      ES.put (n - 1)
      programEffectfulShared
{-# NOINLINE programEffectfulShared #-}

countdownEffectfulShared :: Integer -> (Integer, Integer)
countdownEffectfulShared n = E.runPureEff . ES.runState n $ programEffectfulShared

countdownEffectfulSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ES.runState n
  . runR . runR . runR . runR . runR
  $ programEffectfulShared
  where
    runR = E.runReader ()

----------------------------------------
-- effectful (dynamic)

programEffectfulDynamic :: ED.State Integer E.:> es => E.Eff es Integer
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
  E.runPureEff . ED.runStateLocal n $ programEffectfulDynamic

countdownEffectfulDynShared :: Integer -> (Integer, Integer)
countdownEffectfulDynShared n =
  E.runPureEff . ED.runStateShared n $ programEffectfulDynamic

countdownEffectfulDynLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulDynLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ED.runStateLocal n
  . runR . runR . runR . runR . runR
  $ programEffectfulDynamic
  where
    runR = E.runReader ()

countdownEffectfulDynSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulDynSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ED.runStateShared n
  . runR . runR . runR . runR . runR
  $ programEffectfulDynamic
  where
    runR = E.runReader ()

----------------------------------------
-- efectful (double-dynamic)

runDoubleStateLocal :: s -> E.Eff (ED.State s : es) a -> E.Eff es (a, s)
runDoubleStateLocal s0 = E.reinterpret (ED.runStateLocal s0) $ \env -> \case
  ED.Get      -> ED.get
  ED.Put s    -> ED.put s
  ED.State f  -> ED.state f
  ED.StateM f -> E.localSeqUnlift env $ \unlift -> ED.stateM (unlift . f)

countdownEffectfulDoubleDynLocal :: Integer -> (Integer, Integer)
countdownEffectfulDoubleDynLocal n =
  E.runPureEff . runDoubleStateLocal n $ programEffectfulDynamic

countdownEffectfulDoubleDynLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulDoubleDynLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . runDoubleStateLocal n
  . runR . runR . runR . runR . runR
  $ programEffectfulDynamic
  where
    runR = E.runReader ()

runDoubleStateShared :: s -> E.Eff (ED.State s : es) a -> E.Eff es (a, s)
runDoubleStateShared s0 = E.reinterpret (ED.runStateShared s0) $ \env -> \case
  ED.Get      -> ED.get
  ED.Put s    -> ED.put s
  ED.State f  -> ED.state f
  ED.StateM f -> E.localSeqUnlift env $ \unlift -> ED.stateM (unlift . f)

countdownEffectfulDoubleDynShared :: Integer -> (Integer, Integer)
countdownEffectfulDoubleDynShared n =
  E.runPureEff . runDoubleStateShared n $ programEffectfulDynamic

countdownEffectfulDoubleDynSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulDoubleDynSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . runDoubleStateShared n
  . runR . runR . runR . runR . runR
  $ programEffectfulDynamic
  where
    runR = E.runReader ()

----------------------------------------
-- effectful (labeled-dynamic)

programEffectfulLabeledDynamic
  :: E.Labeled "s" (ED.State Integer) E.:> es
  => E.Eff es Integer
programEffectfulLabeledDynamic = do
  n <- E.labeled @"s" @(ED.State Integer) $ ED.get @Integer
  if n <= 0
    then pure n
    else do
      E.labeled @"s" @(ED.State Integer) $ ED.put (n - 1)
      programEffectfulLabeledDynamic
{-# NOINLINE programEffectfulLabeledDynamic #-}

countdownEffectfulLabeledDynLocal :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynLocal n =
  E.runPureEff . E.runLabeled @"s" (ED.runStateLocal n) $ programEffectfulLabeledDynamic

countdownEffectfulLabeledDynShared :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynShared n =
  E.runPureEff . E.runLabeled @"s" (ED.runStateShared n) $ programEffectfulLabeledDynamic

countdownEffectfulLabeledDynLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . E.runLabeled @"s" (ED.runStateLocal n)
  . runR . runR . runR . runR . runR
  $ programEffectfulLabeledDynamic
  where
    runR = E.runReader ()

countdownEffectfulLabeledDynSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . E.runLabeled @"s" (ED.runStateShared n)
  . runR . runR . runR . runR . runR
  $ programEffectfulLabeledDynamic
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
-- cleff

#ifdef VERSION_cleff

programCleff :: C.State Integer C.:> es => C.Eff es Integer
programCleff = do
  n <- C.get @Integer
  if n <= 0
    then pure n
    else do
      C.put (n - 1)
      programCleff
{-# NOINLINE programCleff #-}

countdownCleffLocal :: Integer -> (Integer, Integer)
countdownCleffLocal n = C.runPure . C.runStateLocal n $ programCleff

countdownCleffLocalDeep :: Integer -> (Integer, Integer)
countdownCleffLocalDeep n = C.runPure
  . runR . runR . runR . runR . runR
  . C.runStateLocal n
  . runR . runR . runR . runR . runR
  $ programCleff
  where
    runR = C.runReader ()

countdownCleffIORef :: Integer -> (Integer, Integer)
countdownCleffIORef n = C.runPure . C.runState n $ programCleff

countdownCleffIORefDeep :: Integer -> (Integer, Integer)
countdownCleffIORefDeep n = C.runPure
  . runR . runR . runR . runR . runR
  . C.runState n
  . runR . runR . runR . runR . runR
  $ programCleff
  where
    runR = C.runReader ()

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
