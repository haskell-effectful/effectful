-- | Provider of the 'MonadPrim' instance for 'Eff'.
module Effectful.Prim
  ( -- * Effect
    Prim
  , PrimStateEff

    -- ** Handlers
  , runPrim
  ) where

import Effectful.Internal.Monad
