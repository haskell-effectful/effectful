-- | The 'Writer' effect.
module Effectful.Writer
  ( Writer
  , runWriter
  , execWriter
  , tell
  , listen
  , listens
  ) where

import Control.Exception

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), thread local, write only value of type
-- @w@.
newtype Writer w :: Effect where
  Writer :: w -> Writer w m r

runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  (a, IdE (Writer w)) <- runEffect (IdE (Writer mempty)) m
  pure (a, w)

execWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriter m = do
  IdE (Writer w) <- execEffect (IdE (Writer mempty)) m
  pure w

tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w = stateEffect $ \(IdE (Writer w0)) -> ((), IdE (Writer (w0 <> w)))

listen :: (Writer w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen m = unsafeEff $ \es -> mask $ \restore -> do
  w0 <- unsafeStateEnv (\(IdE (Writer w)) -> (w, IdE (Writer mempty))) es
  a <- restore (unEff m es) `onException` merge es w0
  (a, ) <$> merge es w0
  where
    merge es w0 =
      -- If an exception is thrown, restore w0 and keep parts of w1.
      unsafeStateEnv (\(IdE (Writer w1)) -> (w1, IdE (Writer (w0 <> w1)))) es

listens :: (Writer w :> es, Monoid w) => (w -> b) -> Eff es a -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
