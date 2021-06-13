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

import Data.Coerce
import qualified Data.Semigroup as S

import Effectful.Internal.Has
import Effectful.Internal.Monad

-- | Provide access to a write only value of type @w@.
newtype Writer w = Writer w
  deriving (S.Semigroup, Monoid)

runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter = fmap coerce . runEffect mempty

execWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriter = fmap coerce . execEffect mempty

writer :: (Writer w :> es, Monoid w) => (a, w) -> Eff es a
writer (a, w) = stateEffect $ \w0 -> (a, w0 `mappend` Writer w)

tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w = stateEffect $ \w0 -> ((), w0 `mappend` Writer w)

listen
  :: (Writer w :> es, Monoid w)
  => Eff es a
  -> Eff es (a, w)
listen = fmap (\(a, Writer w) -> (a, w)) . listenEffect

listens
  :: (Writer w :> es, Monoid w)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
