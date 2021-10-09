module Effectful.Handler
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
  (`unEff` es) $ runHandler (Handler les handler) m

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
    (`unEff` es) $ runHandler (Handler les handler) m

----------------------------------------
-- Unlifts

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnlift'.
localSeqUnlift
  :: HasCallStack
  => LocalEnv localEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnlift (LocalEnv les) k = unsafeEff $ \es -> do
  seqUnliftEff les $ \unlift -> do
    (`unEff` es) $ k $ unsafeEff_ . unlift

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnliftIO'.
localSeqUnliftIO
  :: (HasCallStack, IOE :> es)
  => LocalEnv localEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) k = liftIO $ seqUnliftEff les k

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: HasCallStack
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    seqUnliftEff les $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
  ConcUnlift p l -> unsafeEff $ \es -> do
    concUnliftEff les p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: (HasCallStack, IOE :> es)
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnliftIO (LocalEnv les) strategy k = case strategy of
  SeqUnlift      -> liftIO $ seqUnliftEff les k
  ConcUnlift p l -> liftIO $ concUnliftEff les p l k

-- | Utility for lifting 'Eff' operations of type
--
-- @'Eff' es a -> 'Eff' es b@
--
-- to
--
-- @forall localEs. 'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the operation must not run its argument in a separate thread,
-- attempting to do so will result in a runtime error.
withLiftMap
  :: HasCallStack
  => ((forall a b localEs. (Eff es a -> Eff es b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMap k = unsafeEff $ \es -> do
  (`unEff` es) $ k $ \mapEff m -> unsafeEff $ \localEs -> do
    seqUnliftEff localEs $ \unlift -> do
      (`unEff` es) . mapEff . unsafeEff_ $ unlift m

-- | Utility for lifting 'IO' operations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @forall localEs. 'Eff' localEs a -> 'Eff' localEs b@
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
-- runFork = interpret $ \env (ForkWithUnmask m) -> withLiftMapIO $ \liftMap -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
-- :}
withLiftMapIO
  :: (HasCallStack, IOE :> es)
  => ((forall a b localEs. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMapIO k = k $ \mapIO m -> unsafeEff $ \es -> do
  seqUnliftEff es $ \unlift -> mapIO $ unlift m

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
  :: HasCallStack
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff es r -> Eff localEs r) -> (forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    seqUnliftEff es $ \unliftEs -> do
      seqUnliftEff les $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)
  ConcUnlift p l -> unsafeEff $ \es -> do
    concUnliftEff es p l $ \unliftEs -> do
      concUnliftEff les p l $ \unliftLocalEs -> do
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
  :: (HasCallStack, IOE :> es)
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. IO r -> Eff localEs r) -> (forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnliftIO (LocalEnv les) strategy k = case strategy of
  SeqUnlift      -> liftIO $ seqUnliftEff les $ k unsafeEff_
  ConcUnlift p l -> liftIO $ concUnliftEff les p l $ k unsafeEff_

-- $setup
-- >>> import Control.Concurrent
