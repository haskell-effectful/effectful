-- | Provider of the 'MonadFail' instance for 'Eff'.
module Effectful.Fail
  ( -- * Effect
    Fail(..)

    -- ** Handlers
  , runFail
  , runFailIO
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Internal.Monad (Fail(..))

-- | Run the 'Fail' effect via 'Error'.
runFail :: HasCallStack => Eff (Fail : es) a -> Eff es (Either String a)
runFail = reinterpret_ runErrorNoCallStack $ \case
  Fail msg -> throwError msg

-- | Run the 'Fail' effect via the 'MonadFail' instance for 'IO'.
runFailIO :: (HasCallStack, IOE :> es) => Eff (Fail : es) a -> Eff es a
runFailIO = interpret_ $ \case
  Fail msg -> liftIO $ fail msg
