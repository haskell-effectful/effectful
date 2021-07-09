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
  , withLiftH
  ) where

import Control.Exception
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Interpreter

-- | Representation of the local 'Eff' environment.
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
  -- ^ Local environment from the effect handler.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnlift (LocalEnv les) f = unsafeEff $ \es -> do
  unEff (seqUnliftEff $ \k -> unEff (f $ unsafeEff_ . k) es) les

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnliftIO'.
localSeqUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) f = unsafeEff_ $ unEff (seqUnliftEff f) les

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnlift (LocalEnv les) unlift f = unsafeEff $ \es -> case unlift of
  SeqUnlift      -> unEff (seqUnliftEff $ \k -> unEff (f $ unsafeEff_ . k) es) les
  ConcUnlift p b -> unEff (concUnliftEff p b $ \k -> unEff (f $ unsafeEff_ . k) es) les

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnliftIO (LocalEnv les) unlift f = unsafeEff_ $ case unlift of
  SeqUnlift      -> unEff (seqUnliftEff f) les
  ConcUnlift p b -> unEff (concUnliftEff p b f) les

-- | Create a local higher order lifting function.
--
-- Necessary for lifting 'Control.Concurrent.forkIOWithUnmask'-like operations:
--
-- >>> :{
-- data Fork :: Effect where
--   ForkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> Fork m ThreadId
-- :}
--
-- >>> :{
-- runFork :: IOE :> es => Eff (Fork : es) a -> Eff es a
-- runFork = interpretM $ \env (ForkWithUnmask m) -> withLiftH $ \liftH -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftH unmask
-- :}
withLiftH
  :: IOE :> es
  => ((forall a b localEs. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the higher order lifting function in scope.
  -> Eff es r
withLiftH k = k $ \f m -> unsafeEff $ f . unEff m

-- $setup
-- >>> import Control.Concurrent
-- >>> import Effectful
