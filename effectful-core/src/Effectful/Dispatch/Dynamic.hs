module Effectful.Dispatch.Dynamic
  ( -- * Sending operations to the handler
    send
  , EffectStyle

  -- * Handling effects
  , EffectHandler
  , DynamicEffect(..)
  , runDynamic
  , rerunDynamic
  , interpretDynamic
  , reinterpretDynamic

  -- ** Handling local 'Eff' computations
  , LocalEnv

  -- *** Unlifts
  , localSeqUnlift
  , localSeqUnliftIO
  , localUnlift
  , localUnliftIO

  -- *** Lifts
  , withLiftMap
  , withLiftMapIO

  -- *** Bidirectional lifts
  , localLiftUnlift
  , localLiftUnliftIO

  -- *** Utils
  , SuffixOf

  -- * Re-exports
  , HasCallStack
  ) where

import Control.Monad.IO.Unlift
import Data.Kind
import GHC.Stack (HasCallStack)

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Handling effects

runDynamic
  :: (EffectStyle e ~ DynamicEffect, HasCallStack)
  => (forall es' a. HasCallStack => e (Eff es') a -> Eff es a)
  -> Eff (e : es) x -> Eff es x
runDynamic f = interpretDynamic (const f)

rerunDynamic
  :: EffectStyle e ~ DynamicEffect
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> (forall es' x. HasCallStack => e (Eff es') x -> Eff handlerEs x)
  -> Eff (e : es) a -> Eff es b
rerunDynamic inF f = reinterpretDynamic inF (const f)

-- | Interpret an effect.
interpretDynamic
  :: (EffectStyle e ~ DynamicEffect)
  => EffectHandler e es
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpretDynamic handler m = unsafeEff $ \es -> do
  les <- forkEnv es
  (`unEff` es) $ runHandler (DynamicEffect les handler) m

-- | Interpret an effect using other effects.
reinterpretDynamic
  :: (EffectStyle e ~ DynamicEffect)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpretDynamic runHandlerEs handler m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runHandlerEs . unsafeEff $ \les -> do
    (`unEff` es) $ runHandler (DynamicEffect les handler) m

----------------------------------------
-- Unlifts

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnlift'.
localSeqUnlift
  :: (HasCallStack, SuffixOf es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnlift (LocalEnv les) k = unsafeEff $ \es -> do
  seqUnliftIO les $ \unlift -> do
    (`unEff` es) $ k $ unsafeEff_ . unlift

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnliftIO'.
localSeqUnliftIO
  :: (HasCallStack, SuffixOf es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) k = liftIO $ seqUnliftIO les k

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: (HasCallStack, SuffixOf es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    seqUnliftIO les $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
  ConcUnlift p l -> unsafeEff $ \es -> do
    concUnliftIO les p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: (HasCallStack, SuffixOf es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnliftIO (LocalEnv les) strategy k = case strategy of
  SeqUnlift      -> liftIO $ seqUnliftIO les k
  ConcUnlift p l -> liftIO $ concUnliftIO les p l k

-- | Utility for lifting 'Eff' computations of type
--
-- @'Eff' es a -> 'Eff' es b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the computation must not run its argument in a separate thread,
-- attempting to do so will result in a runtime error.
withLiftMap
  :: (HasCallStack, SuffixOf es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall a b. (Eff es a -> Eff es b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMap !_ k = unsafeEff $ \es -> do
  -- The LocalEnv parameter is not used, but we need it to constraint the
  -- localEs type variable. It's also strict so that callers don't cheat.
  (`unEff` es) $ k $ \mapEff m -> unsafeEff $ \localEs -> do
    seqUnliftIO localEs $ \unlift -> do
      (`unEff` es) . mapEff . unsafeEff_ $ unlift m

-- | Utility for lifting 'IO' computations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the computation must not run its argument in a separate thread,
-- attempting to do so will result in a runtime error.
--
-- Useful e.g. for lifting the unmasking function in
-- 'Control.Exception.mask'-like computations:
--
-- >>> :{
-- data Fork :: Effect where
--   ForkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> Fork m ThreadId
-- :}
--
-- >>> :{
-- runFork :: IOE :> es => Eff (Fork : es) a -> Eff es a
-- runFork = interpretDynamic $ \env (ForkWithUnmask m) -> withLiftMapIO env $ \liftMap -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
-- :}
withLiftMapIO
  :: (HasCallStack, SuffixOf es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall a b. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMapIO !_ k = k $ \mapIO m -> unsafeEff $ \es -> do
  -- The LocalEnv parameter is not used, but we need it to constraint the
  -- localEs type variable. It's also strict so that callers don't cheat.
  seqUnliftIO es $ \unlift -> mapIO $ unlift m

----------------------------------------
-- Bidirectional lifts

-- | Create a local lifting and unlifting function with the given strategy.
--
-- Useful for lifting complicated 'Eff' computations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the computation you're lifting 'localUnlift' along with
-- 'withLiftMap' might be enough and is more efficient.
localLiftUnlift
  :: (HasCallStack, SuffixOf es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff es r -> Eff localEs r) -> (forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    seqUnliftIO es $ \unliftEs -> do
      seqUnliftIO les $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)
  ConcUnlift p l -> unsafeEff $ \es -> do
    concUnliftIO es p l $ \unliftEs -> do
      concUnliftIO les p l $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)

-- | Create a local unlifting function with the given strategy along with an
-- unrestricted lifting function.
--
-- Useful for lifting complicated 'IO' computations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the computation you're lifting 'localUnliftIO' along
-- with 'withLiftMapIO' might be enough and is more efficient.
localLiftUnliftIO
  :: (HasCallStack, SuffixOf es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. IO r -> Eff localEs r) -> (forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnliftIO (LocalEnv les) strategy k = case strategy of
  SeqUnlift      -> liftIO $ seqUnliftIO les $ k unsafeEff_
  ConcUnlift p l -> liftIO $ concUnliftIO les p l $ k unsafeEff_

----------------------------------------
-- Utils

-- | Require that the second list of effects is a suffix of the first one.
--
-- In other words, 'SuffixOf' @es@ @baseEs@ means "a suffix of @es@ is
-- @baseEs@".
type family SuffixOf (es :: [Effect]) (baseEs :: [Effect]) :: Constraint where
  SuffixOf   baseEs baseEs = ()
  SuffixOf (e : es) baseEs = SuffixOf es baseEs

-- $setup
-- >>> import Control.Concurrent
