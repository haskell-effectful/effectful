-- | The 'Writer' effect.
--
-- Represented as an 'MVar' underneath, therefore:
--
-- - shareable between multiple threads,
--
-- - slower than "Effectful.Writer.Local".
--
-- /Warning:/ 'Writer'\'s state will be accumulated via __left-associated__ uses
-- of '<>', which makes it unsuitable for use with types for which such pattern
-- is inefficient. __This applies, in particular, to the standard list type__,
-- which makes the 'Writer' effect pretty niche.
--
-- /Note:/ while the 'Control.Monad.Trans.Writer.Strict.Writer' from the
-- @transformers@ package includes additional operations
-- 'Control.Monad.Trans.Writer.Strict.pass' and
-- 'Control.Monad.Trans.Writer.Strict.censor', they don't cooperate with runtime
-- exceptions very well, so they're deliberately omitted.
module Effectful.Writer.Shared
  ( Writer
  , runWriter
  , execWriter
  , tell
  , listen
  , listens
  ) where

import Control.Concurrent.MVar
import Control.Exception

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Provide access to a strict (WHNF), shareable, write only value of type @w@.
newtype Writer w :: Effect where
  Writer :: MVar w -> Writer w m r

-- | Run a 'Writer' effect and return the final value along with the final
-- output.
runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  v <- unsafeEff_ $ newMVar mempty
  a <- evalEffect (IdE (Writer v)) m
  (a, ) <$> unsafeEff_ (readMVar v)

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value.
execWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriter m = do
  v <- unsafeEff_ $ newMVar mempty
  _ <- evalEffect (IdE (Writer v)) m
  unsafeEff_ $ readMVar v

-- | Append the given output to the overall output of the 'Writer'.
tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w1 = unsafeEff $ \es -> do
  IdE (Writer v) <- getEnv es
  modifyMVar_ v $ \w0 -> let w = w0 <> w1 in w `seq` pure w

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
--
-- /Note:/ if a runtime exception is received while the action is executed, the
-- partial output of the action will still be appended to the overall output of
-- the 'Writer'.
listen :: (Writer w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen m = unsafeEff $ \es -> do
  -- The mask is uninterruptible because modifyMVar_ v0 in the merge function
  -- might block and if an async exception is received while waiting, w1 will be
  -- lost.
  uninterruptibleMask $ \restore -> do
    v1 <- newMVar mempty
    -- Replace thread local MVar with a fresh one for isolated listening.
    v0 <- stateEnv es $ \(IdE (Writer v)) -> (v, IdE (Writer v1))
    a <- restore (unEff m es) `onException` merge es v0 v1
    (a, ) <$> merge es v0 v1
  where
    -- Merge results accumulated in the local MVar with the mainline. If an
    -- exception was received while listening, merge results recorded so far.
    merge es v0 v1 = do
      putEnv es $ IdE (Writer v0)
      w1 <- readMVar v1
      modifyMVar_ v0 $ \w0 -> let w = w0 <> w1 in w `seq` pure w
      pure w1

listens :: (Writer w :> es, Monoid w) => (w -> b) -> Eff es a -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
