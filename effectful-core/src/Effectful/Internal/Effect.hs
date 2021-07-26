{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | Type-safe indexing for 'Effectful.Internal.Monad.Env'.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Effect
  ( Effect
  , (:>)(..)
  , IdE(..)
  ) where

import Data.Kind
import GHC.TypeLits

-- | The kind of effects.
type Effect = (Type -> Type) -> Type -> Type

-- | Adapter for statically dispatched effects.
newtype IdE :: Effect -> Type where
  IdE :: (forall m r. e m r) -> IdE e

-- | A constraint that requires that a particular effect, @e@, is a member of
-- the type-level list @es@. This is used to parameterize an
-- 'Effectful.Monad.Eff' operation over an arbitrary list of effects, so long as
-- @e@ is /somewhere/ in the list.
--
-- For example, an operation that only needs access to a cell of mutable state
-- containing an 'Integer' would likely use the following type:
--
-- @
-- 'Effectful.State.State' 'Integer' ':>' es => 'Effectful.Monad.Eff' es ()
-- @
class (e :: Effect) :> (es :: [Effect]) where
  -- | Get the position of @e@ in @es@.
  --
  -- /Note:/ GHC is kind enough to cache these values as they're top level CAFs,
  -- so the lookup is amortized @O(1)@ without any language level tricks.
  reifyIndex :: Int
  reifyIndex = -- Don't show "minimal complete definition" in haddock.
               error "unimplemented"

instance TypeError
  ('Text "There is no handler for '" ':<>:
   'ShowType e ':<>:
   'Text "' in the context"
  ) => e :> '[] where
  reifyIndex = error "unreachable"

instance {-# OVERLAPPING #-} e :> (e : es) where
  reifyIndex = 0

instance e :> es => e :> (x : es) where
  reifyIndex = 1 + reifyIndex @e @es
