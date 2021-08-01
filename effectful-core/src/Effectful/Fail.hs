module Effectful.Fail
  ( Fail(..)
  , runFail
  ) where

import Effectful.Error
import Effectful.Handler
import Effectful.Internal.Monad

-- | Run the 'Fail' effect via 'Error'.
runFail :: Eff (Fail : es) a -> Eff es (Either String a)
runFail = reinterpret eff $ \_ -> \case
  Fail msg -> throwError msg
  where
    eff = fmap (either (Left . snd) Right) . runError
