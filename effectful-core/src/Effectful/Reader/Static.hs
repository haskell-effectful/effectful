-- | Support for access to a read only value of a particular type.
--
-- /Note:/ strictly speaking the value is not read only because of 'local'. If
-- you want to ensure that the initial value never changes, use
-- "Effectful.Input.Static.Value".
module Effectful.Reader.Static
  ( -- * Effect
    Reader

    -- ** Handlers
  , runReader
  , withReader

    -- ** Operations
  , ask
  , asks
  , local
  ) where

import Data.Kind

import Effectful
import Effectful.Dispatch.Static

-- | Provide access to a strict (WHNF), thread local, read only value of type
-- @r@.
data Reader (r :: Type) :: Effect

type instance DispatchOf (Reader r) = Static NoSideEffects
newtype instance StaticRep (Reader r) = Reader r

-- | Run a 'Reader' effect with the given initial environment.
runReader
  :: HasCallStack
  => r -- ^ The initial environment.
  -> Eff (Reader r : es) a
  -> Eff es a
runReader r = evalStaticRep (Reader r)

-- | Execute a computation in a modified environment.
--
-- @since 1.1.0.0
withReader
  :: HasCallStack
  => (r1 -> r2)
  -- ^ The function to modify the environment.
  -> Eff (Reader r2 : es) a
  -- ^ Computation to run in the modified environment.
  -> Eff (Reader r1 : es) a
withReader f m = do
  r <- ask
  raise $ runReader (f r) m

-- | Fetch the value of the environment.
ask :: (HasCallStack, Reader r :> es) => Eff es r
ask = do
  Reader r <- getStaticRep
  pure r

-- | Retrieve a function of the current environment.
--
-- @'asks' f ≡ f '<$>' 'ask'@
asks
  :: (HasCallStack, Reader r :> es)
  => (r -> a) -- ^ The function to apply to the environment.
  -> Eff es a
asks f = f <$> ask

-- | Execute a computation in a modified environment.
--
-- @'runReader' r ('local' f m) ≡ 'runReader' (f r) m@
--
local
  :: (HasCallStack, Reader r :> es)
  => (r -> r) -- ^ The function to modify the environment.
  -> Eff es a
  -> Eff es a
local f = localStaticRep $ \(Reader r) -> Reader (f r)
