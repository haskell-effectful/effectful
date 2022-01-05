module Effectful.Timeout
  ( Timeout
  , runTimeout
  , timeout
  ) where

import qualified System.Timeout as T

import Effectful.Dispatch.Static
import Effectful.Monad

-- | An effect for timing out computations.
data Timeout :: Effect where
  Timeout :: Timeout m r

type instance EffectStyle Timeout = DataA

-- | Run the 'Timeout' effect.
runTimeout :: IOE :> es => Eff (Timeout : es) a -> Eff es a
runTimeout = evalData (DataA Timeout)

-- | Lifted 'T.timeout'.
timeout
  :: Timeout :> es
  => Int
  -- ^ The timeout in microseconds (1/10^6 seconds).
  -> Eff es a
  -- ^ The computation the timeout applies to.
  -> Eff es (Maybe a)
timeout = unsafeLiftMapIO . T.timeout
