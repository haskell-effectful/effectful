module Effectful.Interpreter
  ( -- * Sending commands
    send

  -- * Handling effects
  , RunIn

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
-- For handling local 'Eff' operations see the 'localUnlift' family.
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
-- For handling local 'Eff' operations see the 'localUnlift' family.
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

-- | Create a local unlifting function with the 'SeqUnlift' strategy.
localSeqUnlift
  :: LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> (RunIn localEs (Eff es) -> Eff es a)
  -- ^ Continuation with the unlifting function.
  -> Eff es a
localSeqUnlift (LocalEnv les) f = unsafeEff $ \es -> do
  unEff (seqUnliftEff $ \k -> unEff (f $ unsafeEff_ . k) es) les

-- | Create a local unlifting function with the 'SeqUnlift' strategy.
localSeqUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> (RunIn localEs IO -> IO a)
  -- ^ Continuation with the unlifting function.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) f = unsafeEff_ $ unEff (seqUnliftEff f) les

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> UnliftStrategy
  -> (RunIn localEs (Eff es) -> Eff es a)
  -- ^ Continuation with the unlifting function.
  -> Eff es a
localUnlift (LocalEnv les) unlift f = unsafeEff $ \es -> case unlift of
  SeqUnlift ->
    unEff (seqUnliftEff $ \k -> unEff (f $ unsafeEff_ . k) es) les
  BoundedConcUnlift n ->
    unEff (boundedConcUnliftEff n $ \k -> unEff (f $ unsafeEff_ . k) es) les
  UnboundedConcUnlift ->
    unEff (unboundedConcUnliftEff $ \k -> unEff (f $ unsafeEff_ . k) es) les

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: IOE :> es
  => LocalEnv localEs
  -- ^ Local environment from the effect handler.
  -> UnliftStrategy
  -> (RunIn localEs IO -> IO a)
  -- ^ Continuation with the unlifting function.
  -> Eff es a
localUnliftIO (LocalEnv les) unlift f = unsafeEff_ $ case unlift of
  SeqUnlift           -> unEff (seqUnliftEff f) les
  BoundedConcUnlift n -> unEff (boundedConcUnliftEff n f) les
  UnboundedConcUnlift -> unEff (unboundedConcUnliftEff f) les
