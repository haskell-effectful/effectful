-- | The dynamically dispatched variant of the 'Reader' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
-- recommended to use the statically dispatched variant,
-- i.e. "Effectful.Reader.Static".
module Effectful.Reader.Dynamic
  ( -- * Effect
    Reader(..)

    -- ** Handlers
  , runReader

    -- ** Operations
  , ask
  , asks
  , local
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.Reader.Static as R

data Reader r :: Effect where
  Ask   :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

type instance DispatchOf (Reader r) = 'Dynamic

-- | Run the 'Reader' effect with the given initial environment (via
-- "Effectful.Reader.Static").
runReader
  :: r -- ^ The initial environment.
  -> Eff (Reader r : es) a
  -> Eff es a
runReader r = reinterpret (R.runReader r) $ \env -> \case
  Ask       -> R.ask
  Local f m -> localSeqUnlift env $ \unlift -> R.local f (unlift m)

----------------------------------------
-- Operations

-- | Fetch the value of the environment.
ask :: (HasCallStack, Reader r :> es) => Eff es r
ask = send Ask

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
local f = send . Local f
