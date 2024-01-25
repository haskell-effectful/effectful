module Effectful.Timeout
  ( -- * Effect
    Timeout

    -- ** Handlers
  , runTimeout

    -- ** Operations
  , timeout
  ) where

import System.Timeout qualified as T

import Effectful
import Effectful.Dispatch.Static

-- | An effect for timing out computations.
data Timeout :: Effect

type instance DispatchOf Timeout = Static WithSideEffects
data instance StaticRep Timeout = Timeout

-- | Run the 'Timeout' effect.
runTimeout :: IOE :> es => Eff (Timeout : es) a -> Eff es a
runTimeout = evalStaticRep Timeout

-- | Lifted 'T.timeout'.
timeout
  :: Timeout :> es
  => Int
  -- ^ The timeout in microseconds (1/10^6 seconds).
  -> Eff es a
  -- ^ The computation the timeout applies to.
  -> Eff es (Maybe a)
timeout = unsafeLiftMapIO . T.timeout
