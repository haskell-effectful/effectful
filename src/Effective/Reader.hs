-- | The 'Reader' as an effect.
module Effective.Reader
  ( Reader
  , runReader
  , ask
  , local
  , reader
  , asks
  ) where

import Effective.Internal.Has
import Effective.Internal.Monad

-- | Provide access to a read only value of type @r@.
newtype Reader r = Reader { unReader :: r }

runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = evalEffect (Reader r)

ask :: Reader r :> es => Eff es r
ask = unReader <$> getEffect

local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f = localEffect (Reader . f . unReader)

reader :: Reader r :> es => (r -> a) -> Eff es a
reader f = f . unReader <$> getEffect

asks :: Reader r :> es => (r -> a) -> Eff es a
asks = reader
