-- | The 'Reader' effect.
module Effectful.Reader
  ( Reader
  , runReader
  , ask
  , asks
  , local
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), thread local, read only value of type
-- @r@.
newtype Reader r :: Effect where
  Reader :: r -> Reader r m a

runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = evalEffect (IdE (Reader r))

ask :: Reader r :> es => Eff es r
ask = do
  IdE (Reader r) <- getEffect
  pure r

asks :: Reader r :> es => (r -> a) -> Eff es a
asks f = do
  IdE (Reader r) <- getEffect
  pure $ f r

local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f = localEffect $ \(IdE (Reader r)) -> IdE (Reader (f r))
