{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FieldSelectors #-}
-- The deprecated stateM and StateM need to be benchmarked until they're
-- removed.
{-# OPTIONS_GHC -Wno-deprecations #-}
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

-- bluefin
#ifdef VERSION_bluefin
import Bluefin.Capability.Modify qualified as B
import Bluefin.Compound qualified as B
import Bluefin.DslBuilderEff qualified as B
import Bluefin.Eff qualified as B
import Bluefin.Reader qualified as B
import Bluefin.State qualified as B
#endif

-- mtl
#ifdef VERSION_mtl
import Control.Monad.Reader qualified as M
import Control.Monad.State.Strict qualified as M
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

countdownMtlTransformers :: Integer -> (Integer, Integer)
countdownMtlTransformers n = flip M.runState n $ programMtl

countdownMtlTransformersDeep :: Integer -> (Integer, Integer)
countdownMtlTransformersDeep n = runIdentity
  . runR . runR . runR . runR . runR
  . flip M.runStateT n
  . runR . runR . runR . runR . runR
  $ programMtl
  where
    runR = flip M.runReaderT ()

countdownMtlEffectful :: Integer -> (Integer, Integer)
countdownMtlEffectful n = E.runPureEff . ED.runStateLocal n $ programMtl

countdownMtlEffectfulDeep :: Integer -> (Integer, Integer)
countdownMtlEffectfulDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . ED.runStateLocal n
  . runR . runR . runR . runR . runR
  $ programMtl
  where
    runR = E.runReader ()

#endif

----------

#ifdef VERSION_bluefin

data Bluefin_DynamicState s e = Bluefin_DynamicState
  { bluefinDynamicGetImpl :: B.Eff e s
  , bluefinDynamicPutImpl :: s -> B.Eff e ()
  }
  deriving stock B.Generic
  deriving B.Handle via B.OneWayCoercibleHandle (Bluefin_DynamicState s)

instance
  e B.<: es
  => B.OneWayCoercible
    (Bluefin_DynamicState s e)
    (Bluefin_DynamicState s es)
  where
  oneWayCoercibleImpl = B.gOneWayCoercible

bluefinDynamicGet :: e B.<: es => Bluefin_DynamicState s e -> B.Eff es s
bluefinDynamicGet h = B.makeOp
  $ bluefinDynamicGetImpl (B.mapHandle h)

bluefinDynamicPut
  :: e B.<: es
  => Bluefin_DynamicState s e
  -> s
  -> B.Eff es ()
bluefinDynamicPut h s = B.makeOp
  $ bluefinDynamicPutImpl (B.mapHandle h) s

bluefin_runState
  :: s
  -> (forall e. Bluefin_DynamicState s e -> B.Eff (e B.:& es) a)
  -> B.Eff es (a, s)
bluefin_runState n k = B.runModify n
  $ \st ->
  B.useImplIn k Bluefin_DynamicState
    { bluefinDynamicGetImpl = B.get st
    , bluefinDynamicPutImpl = B.put st
    }

instance M.MonadState s (B.DslBuilderEff (Bluefin_DynamicState s) es) where
  get = B.dslBuilderEff bluefinDynamicGet
  put s = B.dslBuilderEff $ \st -> bluefinDynamicPut st s

countdownMtlBluefin :: Integer -> (Integer, Integer)
countdownMtlBluefin n = B.runPureEff $ bluefin_runState n $ \st ->
  B.runDslBuilderEff (B.mapHandle st) programMtl

countdownMtlBluefinDeep :: Integer -> (Integer, Integer)
countdownMtlBluefinDeep n = B.runPureEff $
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  bluefin_runState n $ \st ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runReader () $ \_ ->
  B.runDslBuilderEff (B.mapHandle st) programMtl

-- Bluefin direct-state analogues.
programBluefinLocal :: e B.<: es => B.State Integer e -> B.Eff es Integer
programBluefinLocal st = do
  n <- B.get st
  if n <= 0
    then pure n
    else do
      B.put st (n - 1)
      programBluefinLocal st
{-# NOINLINE programBluefinLocal #-}

programBluefinDynamic :: e B.<: es => Bluefin_DynamicState Integer e -> B.Eff es Integer
programBluefinDynamic st = do
  n <- bluefinDynamicGet st
  if n <= 0
    then pure n
    else do
      bluefinDynamicPut st (n - 1)
      programBluefinDynamic st
{-# NOINLINE programBluefinDynamic #-}

bluefinDeep
  :: (forall e1 e2 e3 e4 e5. B.Eff (e5 B.:& e4 B.:& e3 B.:& e2 B.:& e1 B.:& es) a)
  -> B.Eff es a
bluefinDeep m = B.runReader ()
  $ \_ -> B.runReader ()
  $ \_ -> B.runReader ()
  $ \_ -> B.runReader ()
  $ \_ -> B.runReader ()
  $ \_ -> m

countdownBluefinLocal :: Integer -> (Integer, Integer)
countdownBluefinLocal n = B.runPureEff
  $ B.runState n
  $ \st -> programBluefinLocal st

countdownBluefinLocalDeep :: Integer -> (Integer, Integer)
countdownBluefinLocalDeep n = B.runPureEff
  $ bluefinDeep
  $ B.runState n
  $ \st -> bluefinDeep (programBluefinLocal st)

bluefinRunDynamicLocal
  :: s
  -> (forall e. Bluefin_DynamicState s e -> B.Eff (e B.:& es) a)
  -> B.Eff es (a, s)
bluefinRunDynamicLocal n k = B.runState n
  $ \st -> B.useImplIn k Bluefin_DynamicState
    { bluefinDynamicGetImpl = B.get st
    , bluefinDynamicPutImpl = B.put st
    }

countdownBluefinDynLocal :: Integer -> (Integer, Integer)
countdownBluefinDynLocal n = B.runPureEff
  $ bluefinRunDynamicLocal n programBluefinDynamic

countdownBluefinDynLocalDeep :: Integer -> (Integer, Integer)
countdownBluefinDynLocalDeep n = B.runPureEff
  $ bluefinDeep
  $ bluefinRunDynamicLocal n (\st -> bluefinDeep (programBluefinDynamic st))

bluefinRunDoubleStateLocal
  :: s
  -> (forall e. Bluefin_DynamicState s e -> B.Eff (e B.:& es) a)
  -> B.Eff es (a, s)
bluefinRunDoubleStateLocal n k = bluefinRunDynamicLocal n
  $ \st ->
  B.useImplIn k Bluefin_DynamicState
    { bluefinDynamicGetImpl = bluefinDynamicGet st
    , bluefinDynamicPutImpl = bluefinDynamicPut st
    }

countdownBluefinDoubleDynLocal :: Integer -> (Integer, Integer)
countdownBluefinDoubleDynLocal n = B.runPureEff
  $ bluefinRunDoubleStateLocal n programBluefinDynamic

countdownBluefinDoubleDynLocalDeep :: Integer -> (Integer, Integer)
countdownBluefinDoubleDynLocalDeep n = B.runPureEff
  $ bluefinDeep
  $ bluefinRunDoubleStateLocal n
  $ \st -> bluefinDeep (programBluefinDynamic st)

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
-- effectful (labeled-dynamic-send)

programEffectfulLabeledDynamicSend
  :: E.Labeled "s" (ED.State Integer) E.:> es
  => E.Eff es Integer
programEffectfulLabeledDynamicSend = do
  n <- E.send . E.Labeled @"s" $ ED.Get @Integer
  if n <= 0
    then pure n
    else do
      E.send . E.Labeled @"s" $ ED.Put (n - 1)
      programEffectfulLabeledDynamicSend
{-# NOINLINE programEffectfulLabeledDynamicSend #-}

countdownEffectfulLabeledDynSendLocal :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynSendLocal n =
  E.runPureEff . E.runLabeled @"s" (ED.runStateLocal n) $ programEffectfulLabeledDynamicSend

countdownEffectfulLabeledDynSendShared :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynSendShared n =
  E.runPureEff . E.runLabeled @"s" (ED.runStateShared n) $ programEffectfulLabeledDynamicSend

countdownEffectfulLabeledDynSendLocalDeep :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynSendLocalDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . E.runLabeled @"s" (ED.runStateLocal n)
  . runR . runR . runR . runR . runR
  $ programEffectfulLabeledDynamicSend
  where
    runR = E.runReader ()

countdownEffectfulLabeledDynSendSharedDeep :: Integer -> (Integer, Integer)
countdownEffectfulLabeledDynSendSharedDeep n = E.runPureEff
  . runR . runR . runR . runR . runR
  . E.runLabeled @"s" (ED.runStateShared n)
  . runR . runR . runR . runR . runR
  $ programEffectfulLabeledDynamicSend
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
