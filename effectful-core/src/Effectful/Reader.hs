-- | The 'Reader' effect.
module Effectful.Reader
  ( ReaderE
  , runReaderE
  , ask
  , asks
  , local
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), thread local, read only value of type
-- @r@.
newtype ReaderE r :: Effect where
  ReaderE :: r -> ReaderE r m a

runReaderE :: r -> Eff (ReaderE r : es) a -> Eff es a
runReaderE r = evalEffect (IdE (ReaderE r))

ask :: ReaderE r :> es => Eff es r
ask = do
  IdE (ReaderE r) <- getEffect
  pure r

asks :: ReaderE r :> es => (r -> a) -> Eff es a
asks f = do
  IdE (ReaderE r) <- getEffect
  pure $ f r

local :: ReaderE r :> es => (r -> r) -> Eff es a -> Eff es a
local f = localEffect $ \(IdE (ReaderE r)) -> IdE (ReaderE (f r))
