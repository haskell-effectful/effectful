module Effectful.Interpreter
  ( -- * Sending commands
    send

  -- * Interpretation
  , interpret
  , interpretM
  , interpretIO

  -- * Reinterpretation
  , reinterpret
  , reinterpretM
  , reinterpretIO
  ) where

import Control.Monad.Catch

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

----------------------------------------
-- Interpreter

data Interpreter (e :: Effect) = forall localEs. Interpreter
  { _env       :: Env localEs
  , _interpret :: forall a es. (forall r. Eff es r -> IO r)
               -> e (Eff es) a -> Eff localEs a
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
send :: e :> es => e (Eff es) a -> Eff es a
send op = readerEffectM $ \Interpreter{..} -> do
  unsafeUnliftEff $ \run -> unEff (_interpret run op) _env

----------------------------------------
-- Interpretation

-- | Interpret a first order effect.
interpret
  :: (forall r es. e (Eff es) r -> Eff baseEs r)
  -> Eff (e : baseEs) a
  -> Eff      baseEs  a
interpret f = interpretIO $ \_ -> f

-- | Interpret a higher order effect with help of a natural transformation from
-- 'Eff' to 'Eff'.
interpretM
  :: (forall r es. (forall t. Eff es t -> Eff baseEs t) -> e (Eff es) r -> Eff baseEs r)
  -> Eff (e : baseEs) a
  -> Eff      baseEs  a
interpretM f = interpretIO $ \k -> f $ unsafeEff_ . k

-- | Interpret a higher order effect with help of a natural transformation from
-- 'Eff' to 'IO'.
interpretIO
  :: (forall r es. (forall t. Eff es t -> IO t) -> e (Eff es) r -> Eff baseEs r)
  -> Eff (e : baseEs) a
  -> Eff      baseEs  a
interpretIO f m = unsafeEff $ \es -> do
  les <- forkEnv es
  runInterpreter es (Interpreter les f) m

----------------------------------------
-- Reinterpretation

-- | Reinterpret local effects as a first order effect.
reinterpret
  :: (Eff localEs a -> Eff baseEs b)
  -> (forall r es. e (Eff es) r -> Eff localEs r)
  -> Eff (e : baseEs) a
  -> Eff      baseEs  b
reinterpret runLocal f = reinterpretIO runLocal $ \_ -> f

-- | Reinterpret local effects as a higher order effect with help of a natural
-- transformation from 'Eff' to 'Eff'.
reinterpretM
  :: (Eff localEs a -> Eff baseEs b)
  -> (forall r es. (forall t. Eff es t -> Eff localEs t) -> e (Eff es) r -> Eff localEs r)
  -> Eff (e : baseEs) a
  -> Eff      baseEs  b
reinterpretM runLocal f = reinterpretIO runLocal $ \k -> f $ unsafeEff_ . k

-- | Reinterpret local effects as a higher order effect with help of a natural
-- transformation from 'Eff' to 'IO'.
reinterpretIO
  :: (Eff localEs a -> Eff baseEs b)
  -> (forall r es. (forall t. Eff es t -> IO t) -> e (Eff es) r -> Eff localEs r)
  -> Eff (e : baseEs) a
  -> Eff      baseEs  b
reinterpretIO runLocal f m = unsafeEff $ \es -> do
  les0 <- forkEnv es
  (`unEff` les0) . runLocal . unsafeEff $ \les -> do
    runInterpreter es (Interpreter les f) m
