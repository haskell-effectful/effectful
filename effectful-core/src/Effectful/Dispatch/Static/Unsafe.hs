-- | Unsafe utilities for statically dispatched effects.
module Effectful.Dispatch.Static.Unsafe
  ( reallyUnsafeLiftMapIO
  , reallyUnsafeUnliftIO
  ) where

import Effectful.Internal.Monad
