module Effectful.Interpreter
  ( -- * Sending commands
    send

  -- * Handling effects
  , RunIn

  -- ** Interpretation
  , interpret
  , interpretM
  , interpretIO

  -- ** Reinterpretation
  , reinterpret
  , reinterpretM
  , reinterpretIO
  ) where

import Control.Monad.Catch
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Interpreter

data Interpreter (e :: Effect) = forall localEs. Interpreter
  { _env       :: Env localEs
  , _interpret :: forall a es. HasCallStack
               => RunIn es IO
               -> e (Eff es) a
               -> Eff localEs a
  }

runInterpreter :: Env es -> Interpreter e -> Eff (e : es) a -> IO a
runInterpreter es0 e (Eff m) = do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e relinker es0)
          (unsafeTailEnv size0)
          (\es -> m es)
  where
    relinker :: Relinker Interpreter e
    relinker = Relinker $ \relink Interpreter{..} -> do
      env <- relink _env
      pure Interpreter { _env = env, .. }

----------------------------------------
-- Sending commands

-- | Send an operation of a given effect to the interpreter.
send :: (HasCallStack, e :> es) => e (Eff es) a -> Eff es a
send op = readerEffectM $ \Interpreter{..} -> do
  unsafeUnliftEff $ \run -> unEff (_interpret run op) _env

----------------------------------------
-- Interpretation

-- | Interpret a first order effect.
interpret
  :: (forall r es. HasCallStack => e (Eff es) r -> Eff baseEs r)
  -- ^ The effect handler
  -> Eff (e : baseEs) a
  -> Eff      baseEs  a
interpret interpreter m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les $ \_ -> interpreter) m

-- | Interpret a higher order effect with help of a function running 'Eff'
-- computations in the local 'Eff' of an effect handler.
interpretM
  :: (forall r es. HasCallStack => RunIn es (Eff baseEs) -> e (Eff es) r -> Eff baseEs r)
  -- ^ The effect handler
  -> Eff (e : baseEs) a
  -> Eff      baseEs  a
interpretM interpreter m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les $ \k -> interpreter $ unsafeEff_ . k) m

-- | Interpret a higher order effect with help of a function running 'Eff'
-- computations in 'IO'.
interpretIO
  :: (forall r es. HasCallStack => RunIn es IO -> e (Eff es) r -> Eff baseEs r)
  -- ^ The effect handler
  -> Eff (e : baseEs) a
  -> Eff      baseEs  a
interpretIO interpreter m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les interpreter) m

----------------------------------------
-- Reinterpretation

-- | Interpret a first order effect using local effects.
reinterpret
  :: (Eff localEs a -> Eff baseEs b)
  -- ^ Introduction of local effects
  -> (forall r es. HasCallStack => e (Eff es) r -> Eff localEs r)
  -- ^ The effect handler
  -> Eff (e : baseEs) a
  -> Eff      baseEs  b
reinterpret runLocal interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runLocal . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les $ \_ -> interpreter) m

-- | Interpret a higher order effect using local effects with help of a function
-- running 'Eff' computations in the local 'Eff' of an effect handler.
reinterpretM
  :: (Eff localEs a -> Eff baseEs b)
  -- ^ Introduction of local effects
  -> (forall r es. HasCallStack => RunIn es (Eff localEs) -> e (Eff es) r -> Eff localEs r)
  -- ^ The effect handler
  -> Eff (e : baseEs) a
  -> Eff      baseEs  b
reinterpretM runLocal interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runLocal . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les $ \k -> interpreter $ unsafeEff_ . k) m

-- | Interpret a higher order effect using local effects with help of a function
-- running 'Eff' computations in 'IO'.
reinterpretIO
  :: (Eff localEs a -> Eff baseEs b)
  -- ^ Introduction of local effects
  -> (forall r es. HasCallStack => RunIn es IO -> e (Eff es) r -> Eff localEs r)
  -- ^ The effect handler
  -> Eff (e : baseEs) a
  -> Eff      baseEs  b
reinterpretIO runLocal interpreter m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runLocal . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les interpreter) m
