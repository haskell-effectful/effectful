-- | Provider of the 'MonadFail' instance for 'Eff'.
module Effectful.Fail
  ( Fail(..)
  , runFail
  ) where

import Effectful.Dispatch.Dynamic
import Effectful.Error
import Effectful.Internal.Monad

-- | Run the 'Fail' effect via 'Error'.
runFail :: Eff (Fail : es) a -> Eff es (Either String a)
runFail = rerunDynamic eff $ \case
  Fail msg -> throwError msg
  where
    eff = fmap (either (Left . snd) Right) . runError
