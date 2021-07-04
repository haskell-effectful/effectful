module Effectful.Interpreter
  ( -- * Sending commands
    send

  -- * Handling effects
  , RunIn

  -- ** Interpretation
  , interpret
  , interpretM

  -- ** Reinterpretation
  , reinterpret
  , reinterpretM
  ) where

import Control.Exception
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Interpreter

data Interpreter (e :: Effect) = forall handlerEs. Interpreter
  { _env       :: Env handlerEs
  , _interpret :: forall a localEs. HasCallStack
               => RunIn localEs IO
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
--
-- This is used to connect the definition of an effect to the 'Eff' monad so
-- that it can be used and handled.
send :: (HasCallStack, e :> es) => e (Eff es) a -> Eff es a
send op = readerEffectM $ \Interpreter{..} -> do
  unsafeUnliftEff $ \run -> unEff (_interpret run op) _env

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

-- | Interpret a higher order effect with help of a function running local 'Eff'
-- operations in the context of an effect handler.
interpretM
  :: (forall r localEs. HasCallStack => RunIn localEs (Eff es) -> e (Eff localEs) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpretM interpreter m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les $ \k -> interpreter $ unsafeEff_ . k) m

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
reinterpret runLocal interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runLocal . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les $ \_ -> interpreter) m

-- | Interpret a higher order effect using other effects with help of a function
-- running local 'Eff' operations in the context of an effect handler.
reinterpretM
  :: (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> (forall r localEs. HasCallStack => RunIn localEs (Eff handlerEs) -> e (Eff localEs) r -> Eff handlerEs r)
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpretM runLocal interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runLocal . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les $ \k -> interpreter $ unsafeEff_ . k) m
