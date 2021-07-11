module Effectful.Interpreter
  ( -- * Sending commands
    send

  -- * Handling effects

  -- ** Basic handlers
  , interpret
  , interpretM

  -- ** Derived handlers
  , reinterpret
  , reinterpretM

  -- ** Local operations
  , LocalEnv

  -- *** 'Eff'
  , localSeqUnlift
  , localUnlift

  -- *** 'IO'
  , localSeqUnliftIO
  , localUnliftIO
  , localLiftUnliftIO
  , withLiftSeqOp
  ) where

import Control.Exception
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Interpreter

-- | Opaque representation of the 'Eff' environment at the point of calling the
-- 'send' function, i.e. right before the control is passed to the effect
-- handler.
newtype LocalEnv es = LocalEnv (Env es)

data Interpreter (e :: Effect) = forall handlerEs. Interpreter
  { _env       :: Env handlerEs
  , _interpret :: forall a localEs. HasCallStack
               => LocalEnv localEs
               -> e (Eff localEs) a
               -> Eff handlerEs a
  }

runInterpreter :: Env es -> Interpreter e -> Eff (e : es) a -> IO a
runInterpreter es0 e m = do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e relinker es0)
          (unsafeTailEnv size0)
          (\es -> unEff m es)
  where
    relinker :: Relinker Interpreter e
    relinker = Relinker $ \relink Interpreter{..} -> do
      env <- relink _env
      pure Interpreter { _env = env, .. }

----------------------------------------
-- Sending commands

-- | Send an operation of a given effect to the interpreter.
send :: (HasCallStack, e :> es) => e (Eff es) a -> Eff es a
send op = unsafeEff $ \es -> do
  Interpreter{..} <- getEnv es
  unEff (_interpret (LocalEnv es) op) _env

----------------------------------------
-- Interpretation

-- | Interpret a first order effect.
interpret
  :: (forall r localEs. HasCallStack => e (Eff localEs) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpret interpreter m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les $ \_ -> interpreter) m

-- | Interpret a higher order effect.
--
-- /Note:/ 'LocalEnv' is for handling local 'Eff' operations using a function
-- from the 'localUnlift' family.
interpretM
  :: (forall r localEs. HasCallStack => LocalEnv localEs -> e (Eff localEs) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpretM interpreter m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les interpreter) m

----------------------------------------
-- Reinterpretation

-- | Interpret a first order effect using other effects.
reinterpret
  :: (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> (forall r localEs. HasCallStack => e (Eff localEs) r -> Eff handlerEs r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpret runHandlerEs interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runHandlerEs . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les $ \_ -> interpreter) m

-- | Interpret a higher order effect using other effects.
--
-- /Note:/ 'LocalEnv' is for handling local 'Eff' operations using a function
-- from the 'localUnlift' family.
reinterpretM
  :: (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> (forall r localEs. HasCallStack => LocalEnv localEs -> e (Eff localEs) r -> Eff handlerEs r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpretM runHandlerEs interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runHandlerEs . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les interpreter) m

----------------------------------------
-- Unlifting

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnlift'.
localSeqUnlift
  :: LocalEnv localEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnlift (LocalEnv les) k = unsafeEff $ \es -> do
  unsafeSeqUnliftEff les $ \unlift -> unEff (k $ unsafeEff_ . unlift) es

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnliftIO'.
localSeqUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) k = unsafeEff_ $ unsafeSeqUnliftEff les k

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnlift (LocalEnv les) strategy k = unsafeEff $ \es -> case strategy of
  SeqUnlift -> unsafeSeqUnliftEff les $ \unlift -> do
    unEff (k $ unsafeEff_ . unlift) es
  ConcUnlift p l -> unsafeConcUnliftEff les p l $ \unlift -> do
    unEff (k $ unsafeEff_ . unlift) es

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnliftIO (LocalEnv les) unlift k = unsafeEff_ $ case unlift of
  SeqUnlift      -> unsafeSeqUnliftEff les k
  ConcUnlift p l -> unsafeConcUnliftEff les p l k

-- | Create a local unlifting function with the given strategy along with an
-- unrestricted lifting function.
--
-- Useful for lifting 'IO' operations where the monadic action shows in both
-- positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the operations you're lifting, 'localUnliftIO' along
-- with 'withLiftSeqOp' might be enough and is more efficient.
localLiftUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. IO r -> Eff localEs r) -> (forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnliftIO (LocalEnv les) unlift k = unsafeEff_ $ case unlift of
  SeqUnlift      -> unsafeSeqUnliftEff les $ k unsafeEff_
  ConcUnlift p l -> unsafeConcUnliftEff les p l $ k unsafeEff_

-- | Utility for lifting 'IO' actions of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @forall localEs. 'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the 'IO' operation must not run its argument in a separate thread,
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
-- runFork = interpretM $ \env (ForkWithUnmask m) -> withLiftSeqOp $ \liftSeqOp -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftSeqOp unmask
-- :}
withLiftSeqOp
  :: IOE :> es
  => ((forall a b localEs. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftSeqOp = unsafeWithLiftSeqOp

-- $setup
-- >>> import Control.Concurrent
-- >>> import Effectful
