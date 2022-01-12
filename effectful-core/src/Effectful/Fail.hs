-- | Provider of the 'MonadFail' instance for 'Eff'.
module Effectful.Fail
  ( Fail(..)
  , runFail
  , runFailIO
  ) where

import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Internal.Monad (Fail(..))
import Effectful.Monad

-- | Run the 'Fail' effect via 'Error'.
runFail :: Eff (Fail : es) a -> Eff es (Either String a)
runFail = reinterpret eff $ \_ -> \case
  Fail msg -> throwError msg
  where
    eff = fmap (either (Left . snd) Right) . runError

-- | Run the 'Fail' effect by using the 'MonadFail' instance for 'IO'.
runFailIO :: IOE :> es => Eff (Fail : es) a -> Eff es a
runFailIO = interpret $ \_ -> \case
  Fail msg -> liftIO $ fail msg
