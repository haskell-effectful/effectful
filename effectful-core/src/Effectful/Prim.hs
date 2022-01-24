-- | Provider of the 'MonadPrim' instance for 'Eff'.
module Effectful.Prim
  ( -- * Effect
    Prim

    -- ** Handlers
  , runPrim
  ) where

import Effectful.Internal.Monad
