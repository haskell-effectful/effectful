-- | The 'Writer' as an effect.
module Effectful.Writer
  ( Writer
  , runWriter
  , execWriter
  , writer
  , tell
  , listen
  , listens
  ) where

import Control.Monad.Catch

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide access to a write only value of type @w@.
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

writer :: (Writer w :> es, Monoid w) => (a, w) -> Eff es a
writer (a, w) = stateEffect $ \(IdE (Writer w0)) -> (a, IdE (Writer (w0 <> w)))

tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w = stateEffect $ \(IdE (Writer w0)) -> ((), IdE (Writer (w0 <> w)))

listen
  :: forall w es a. (Writer w :> es, Monoid w)
  => Eff es a
  -> Eff es (a, w)
listen (Eff m) = unsafeEff $ \es -> mask $ \restore -> do
  w0 <- unsafeStateEnv (\(IdE (Writer w)) -> (w, IdE (Writer mempty))) es
  -- If an exception is thrown, restore e0 and keep parts of e1.
  a <- restore (m es) `onException`
    unsafeModifyEnv (\(IdE (Writer w)) -> IdE (Writer (w0 <> w))) es
  w1 <- unsafeStateEnv (\(IdE (Writer w)) -> (w, IdE (Writer (w0 <> w)))) es
  pure (a, w1)

listens
  :: (Writer w :> es, Monoid w)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
