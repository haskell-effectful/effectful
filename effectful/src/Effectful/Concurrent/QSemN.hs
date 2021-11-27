-- | Lifted "Control.Concurrent.QSemN".
module Effectful.Concurrent.QSemN
  ( QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  ) where

import Control.Concurrent.QSemN (QSemN)
import qualified Control.Concurrent.QSemN as Q

import Effectful.Concurrent.Internal
import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Lifted 'Q.newQSemN'.
newQSemN :: Concurrent :> es => Int -> Eff es QSemN
newQSemN = unsafeEff_ . Q.newQSemN

-- | Lifted 'Q.waitQSemN'.
waitQSemN :: Concurrent :> es => QSemN -> Int -> Eff es ()
waitQSemN x = unsafeEff_ . Q.waitQSemN x

-- | Lifted 'Q.signalQSemN'.
signalQSemN :: Concurrent :> es => QSemN -> Int -> Eff es ()
signalQSemN x = unsafeEff_ . Q.signalQSemN x
