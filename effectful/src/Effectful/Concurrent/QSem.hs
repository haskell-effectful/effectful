-- | Lifted "Control.Concurrent.QSem".
module Effectful.Concurrent.QSem
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * QSem
  , QSem
  , newQSem
  , waitQSem
  , signalQSem
  ) where

import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as Q

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static

-- | Lifted 'Q.newQSem'.
newQSem :: Concurrent :> es => Int -> Eff es QSem
newQSem = unsafeEff_ . Q.newQSem

-- | Lifted 'Q.waitQSem'.
waitQSem :: Concurrent :> es => QSem -> Eff es ()
waitQSem = unsafeEff_ . Q.waitQSem

-- | Lifted 'Q.signalQSem'.
signalQSem :: Concurrent :> es => QSem -> Eff es ()
signalQSem = unsafeEff_ . Q.signalQSem
