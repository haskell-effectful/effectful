-- | Support for access to a write only value of a particular type, which is:
--
-- - thread local (if you want the value to be shared between threads, have a
--   look at "Effectful.Writer.Shared"),
--
-- - very fast.
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
-- exceptions very well, so they're deliberately omitted here.
module Effectful.Writer.Local
  ( Writer
  , runWriter
  , execWriter
  , tell
  , listen
  , listens
  ) where

import Control.Exception (onException, mask)

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Provide access to a strict (WHNF), thread local, write only value of type
-- @w@.
newtype Writer w :: Effect where
  Writer :: w -> Writer w m r

type instance EffectStyle (Writer w) = StaticEffect

-- | Run a 'Writer' effect and return the final value along with the final
-- output.
runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  (a, StaticEffect (Writer w)) <- runData (Writer mempty) m
  pure (a, w)

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value.
execWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriter m = do
  StaticEffect (Writer w) <- execData (StaticEffect (Writer mempty)) m
  pure w

-- | Append the given output to the overall output of the 'Writer'.
tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w = stateData $ \(StaticEffect (Writer w0)) -> ((), StaticEffect (Writer (w0 <> w)))

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
--
-- /Note:/ if an exception is received while the action is executed, the partial
-- output of the action will still be appended to the overall output of the
-- 'Writer':
--
-- >>> :{
--   runEff . execWriter @String $ do
--     tell "Hi"
--     handle (\(_::ErrorCall) -> pure ((), "")) $ do
--       tell " there"
--       listen $ do
--         tell "!"
--         error "oops"
-- :}
-- "Hi there!"
listen :: (Writer w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen m = unsafeEff $ \es -> mask $ \restore -> do
  w0 <- stateEnv es $ \(StaticEffect (Writer w)) -> (w, StaticEffect (Writer mempty))
  a <- restore (unEff m es) `onException` merge es w0
  (a, ) <$> merge es w0
  where
    merge es w0 =
      -- If an exception is thrown, restore w0 and keep parts of w1.
      stateEnv es $ \(StaticEffect (Writer w1)) -> (w1, StaticEffect (Writer (w0 <> w1)))

-- | Execute an action and append its output to the overall output of the
-- 'Writer', then return the final value along with a function of the recorded
-- output.
--
-- @'listens' f m ≡ 'Data.Bifunctor.second' f '<$>' 'listen' m@
listens :: (Writer w :> es, Monoid w) => (w -> b) -> Eff es a -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)

-- $setup
-- >>> import Control.Exception (ErrorCall)
-- >>> import Control.Monad.Catch
