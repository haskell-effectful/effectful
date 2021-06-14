{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | Type-safe indexing for 'Effectful.Internal.Monad.Env'.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Has
  ( Effect
  , (:>)
  , ixEnv
  ) where

import Data.Kind

type Effect = Type

class (e :: Effect) :> (es :: [Effect]) where
  -- | Get position of @e@ in @es@.
  --
  -- /Note:/ GHC is kind enough to cache these values as they're top level CAFs,
  -- so the lookup is amortized @O(1)@ without any language level tricks.
  ixOf :: Int
  ixOf = -- Don't show "minimal complete definition" in haddock.
         error "unimplemented"

instance {-# OVERLAPPING #-} e :> (e : es) where
  ixOf = 0

instance e :> es => e :> (x : es) where
  ixOf = 1 + ixOf @e @es

-- | Get position of @e@ in the 'Effectful.Internal.Env.Env'.
ixEnv :: forall e es. e :> es => Int -> Int
ixEnv n = n - ixOf @e @es - 1
