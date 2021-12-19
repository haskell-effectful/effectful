-- | Support for access to a read only value of a particular type.
module Effectful.Reader
  ( Reader
  , runReader
  , ask
  , asks
  , local
  ) where

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Provide access to a strict (WHNF), thread local, read only value of type
-- @r@.
newtype Reader r :: Effect where
  Reader :: r -> Reader r m a

-- | Run a 'Reader' effect with the given initial environment.
runReader
  :: r -- ^ An initial environment.
  -> Eff (Reader r : es) a
  -> Eff es a
runReader r = evalEffect (IdE (Reader r))

-- | Fetch the value of the environment.
ask :: Reader r :> es => Eff es r
ask = do
  IdE (Reader r) <- getEffect
  pure r

-- | Retrieve a function of the current environment.
--
-- @'asks' f ≡ f '<$>' 'ask'@
asks
  :: Reader r :> es
  => (r -> a) -- ^ The function to apply to the environment.
  -> Eff es a
asks f = f <$> ask

-- | Execute a computation in a modified environment.
--
-- @'runReader' r ('local' f m) ≡ 'runReader' (f r) m@
--
local
  :: Reader r :> es
  => (r -> r) -- ^ The function to modify the environment.
  -> Eff es a
  -> Eff es a
local f = localEffect $ \(IdE (Reader r)) -> IdE (Reader (f r))
