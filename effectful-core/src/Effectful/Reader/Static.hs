-- | Support for access to a read only value of a particular type.
module Effectful.Reader.Static
  ( Reader
  , runReader
  , ask
  , asks
  , local
  ) where

import Effectful
import Effectful.Dispatch.Static

-- | Provide access to a strict (WHNF), thread local, read only value of type
-- @r@.
data Reader r :: Effect

type instance DispatchOf (Reader r) = 'Static
newtype instance StaticRep (Reader r) = Reader r

-- | Run a 'Reader' effect with the given initial environment.
runReader
  :: r -- ^ An initial environment.
  -> Eff (Reader r : es) a
  -> Eff es a
runReader r = evalStaticRep (Reader r)

-- | Fetch the value of the environment.
ask :: Reader r :> es => Eff es r
ask = do
  Reader r <- getStaticRep
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
local f = localStaticRep $ \(Reader r) -> Reader (f r)
