module Effectful.Timeout
  ( Timeout
  , runTimeout
  , timeout
  ) where

import qualified System.Timeout as T

import Effectful.Dispatch.Static
import Effectful.Monad

-- | An effect for timing out computations.
data Timeout :: Effect

type instance DispatchOf Timeout = 'Static
data instance DataR Timeout = Timeout

-- | Run the 'Timeout' effect.
runTimeout :: IOE :> es => Eff (Timeout : es) a -> Eff es a
runTimeout = evalData Timeout

-- | Lifted 'T.timeout'.
timeout
  :: Timeout :> es
  => Int
  -- ^ The timeout in microseconds (1/10^6 seconds).
  -> Eff es a
  -- ^ The computation the timeout applies to.
  -> Eff es (Maybe a)
timeout = unsafeLiftMapIO . T.timeout
