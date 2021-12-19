module Effectful.Dispatch.Dynamic
  ( -- * Sending operations to the handler
    send

  -- * Handling effects
  , EffectHandler
  , interpret
  , reinterpret

  -- ** Handling local 'Eff' operations
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
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Handling effects

-- | Interpret an effect.
interpret
  :: EffectHandler e es
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpret handler m = unsafeEff $ \es -> do
  les <- forkEnv es
  (`unEff` es) $ runHandlerA (HandlerA les handler) m

-- | Interpret an effect using other effects.
reinterpret
  :: (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpret runHandlerEs handler m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runHandlerEs . unsafeEff $ \les -> do
    (`unEff` es) $ runHandlerA (HandlerA les handler) m

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

-- | Utility for lifting 'Eff' operations of type
--
-- @'Eff' es a -> 'Eff' es b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the operation must not run its argument in a separate thread,
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

-- | Utility for lifting 'IO' operations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the operation must not run its argument in a separate thread,
-- attempting to do so will result in a runtime error.
--
-- Useful e.g. for lifting the unmasking function in
-- 'Control.Exception.mask'-like operations:
--
-- >>> :{
-- data Fork :: Effect where
--   ForkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> Fork m ThreadId
-- :}
--
-- >>> :{
-- runFork :: IOE :> es => Eff (Fork : es) a -> Eff es a
-- runFork = interpret $ \env (ForkWithUnmask m) -> withLiftMapIO env $ \liftMap -> do
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
-- Useful for lifting complicated 'Eff' operations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the operation you're lifting 'localUnlift' along with
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
-- Useful for lifting complicated 'IO' operations where the monadic action shows
-- in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the operation you're lifting 'localUnliftIO' along with
-- 'withLiftMapIO' might be enough and is more efficient.
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

-- $setup
-- >>> import Control.Concurrent
