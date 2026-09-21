-- | Provider of the t'Control.Monad.Primitive.MonadPrim' instance for 'Eff'.
module Effectful.Prim
  ( -- * Effect
    Prim
  , PrimStateEff

    -- ** Handlers
  , runPrim
  ) where

import Effectful.Internal.Monad
