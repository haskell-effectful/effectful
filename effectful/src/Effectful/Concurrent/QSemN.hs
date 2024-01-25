-- | Lifted "Control.Concurrent.QSemN".
module Effectful.Concurrent.QSemN
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * QSemN
  , QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  ) where

import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as Q

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static

-- | Lifted 'Q.newQSemN'.
newQSemN :: Concurrent :> es => Int -> Eff es QSemN
newQSemN = unsafeEff_ . Q.newQSemN

-- | Lifted 'Q.waitQSemN'.
waitQSemN :: Concurrent :> es => QSemN -> Int -> Eff es ()
waitQSemN x = unsafeEff_ . Q.waitQSemN x

-- | Lifted 'Q.signalQSemN'.
signalQSemN :: Concurrent :> es => QSemN -> Int -> Eff es ()
signalQSemN x = unsafeEff_ . Q.signalQSemN x
