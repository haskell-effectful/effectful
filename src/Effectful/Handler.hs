module Effectful.Handler
  ( -- * Sending operations to the handler
    send

  -- * Handling effects
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
import qualified Control.Exception as E

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Handler

-- | Opaque representation of the 'Eff' environment at the point of calling the
-- 'send' function, i.e. right before the control is passed to the effect
-- handler.
newtype LocalEnv es = LocalEnv (Env es)

data Handler (e :: Effect) = forall handlerEs. Handler
  { _env    :: Env handlerEs
  , _handle :: forall a localEs. HasCallStack
            => LocalEnv localEs
            -> e (Eff localEs) a
            -> Eff handlerEs a
  }

runHandler :: Env es -> Handler e -> Eff (e : es) a -> IO a
runHandler es0 e m = do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e relinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)
  where
    relinker :: Relinker Handler e
    relinker = Relinker $ \relink Handler{..} -> do
      env <- relink _env
      pure Handler { _env = env, .. }

----------------------------------------
-- Sending operations

-- | Send an operation of a given effect to its handler for execution.
send :: (HasCallStack, e :> es) => e (Eff es) a -> Eff es a
send op = unsafeEff $ \es -> do
  Handler{..} <- getEnv es
  unEff (_handle (LocalEnv es) op) _env

----------------------------------------
-- Interpretation

-- | Interpret an effect.
--
-- /Note:/ 'LocalEnv' is for handling local 'Eff' operations using a function
-- from the 'localUnlift' family.
interpret
  :: (forall r localEs. HasCallStack => LocalEnv localEs -> e (Eff localEs) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpret handler m = unsafeEff $ \es -> do
  les <- forkEnv es
  runHandler es (Handler les handler) m

----------------------------------------
-- Reinterpretation

-- | Interpret an effect using other effects.
--
-- /Note:/ 'LocalEnv' is for handling local 'Eff' operations using a function
-- from the 'localUnlift' family.
reinterpret
  :: (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> (forall r localEs. HasCallStack => LocalEnv localEs -> e (Eff localEs) r -> Eff handlerEs r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpret runHandlerEs handler m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runHandlerEs . unsafeEff $ \les -> do
    runHandler es (Handler les handler) m

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
  unsafeSeqUnliftEff les $ \unlift -> do
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
localSeqUnliftIO (LocalEnv les) k = liftIO $ unsafeSeqUnliftEff les k

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
    unsafeSeqUnliftEff les $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
  ConcUnlift p l -> unsafeEff $ \es -> do
    unsafeConcUnliftEff les p l $ \unlift -> do
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
  SeqUnlift      -> liftIO $ unsafeSeqUnliftEff les k
  ConcUnlift p l -> liftIO $ unsafeConcUnliftEff les p l k

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
  :: (HasCallStack, IOE :> es)
  => ((forall a b localEs. (Eff es a -> Eff es b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMap k = unsafeEff $ \es -> do
  (`unEff` es) $ k $ \mapEff m -> unsafeEff $ \localEs -> do
    unsafeSeqUnliftEff localEs $ \unlift -> do
      (`unEff` es) . mapEff . liftIO $ unlift m

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
-- runFork = interpretM $ \env (ForkWithUnmask m) -> withLiftMapIO $ \liftMap -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
-- :}
withLiftMapIO
  :: (HasCallStack, IOE :> es)
  => ((forall a b localEs. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMapIO k = k $ \mapIO m -> unsafeEff $ \es -> do
  unsafeSeqUnliftEff es $ \unlift -> mapIO $ unlift m

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
  :: (HasCallStack, IOE :> es)
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff es r -> Eff localEs r) -> (forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    unsafeSeqUnliftEff es $ \unliftEs -> do
      unsafeSeqUnliftEff les $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (liftIO . unliftLocalEs)
  ConcUnlift p l -> unsafeEff $ \es -> do
    unsafeConcUnliftEff es p l $ \unliftEs -> do
      unsafeConcUnliftEff les p l $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (liftIO . unliftLocalEs)

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
  SeqUnlift      -> liftIO $ unsafeSeqUnliftEff les $ k unsafeEff_
  ConcUnlift p l -> liftIO $ unsafeConcUnliftEff les p l $ k unsafeEff_

-- $setup
-- >>> import Control.Concurrent
-- >>> import Effectful
