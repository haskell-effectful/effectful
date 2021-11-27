-- | Lifted "Control.Concurrent.QSem".
module Effectful.Concurrent.QSem
  ( QSem
  , newQSem
  , waitQSem
  , signalQSem
  ) where

import Control.Concurrent.QSem (QSem)
import qualified Control.Concurrent.QSem as Q

import Effectful.Concurrent.Effect
import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Lifted 'Q.newQSem'.
newQSem :: Concurrent :> es => Int -> Eff es QSem
newQSem = unsafeEff_ . Q.newQSem

-- | Lifted 'Q.waitQSem'.
waitQSem :: Concurrent :> es => QSem -> Eff es ()
waitQSem = unsafeEff_ . Q.waitQSem

-- | Lifted 'Q.signalQSem'.
signalQSem :: Concurrent :> es => QSem -> Eff es ()
signalQSem = unsafeEff_ . Q.signalQSem
