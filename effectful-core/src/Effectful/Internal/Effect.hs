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
  , (:>>)

  -- * Re-exports
  , Type
  ) where

import Data.Kind
import GHC.TypeLits

-- | The kind of effects.
type Effect = (Type -> Type) -> Type -> Type

-- | A constraint that requires that a particular effect @e@ is a member of the
-- type-level list @es@. This is used to parameterize an 'Effectful.Eff'
-- computation over an arbitrary list of effects, so long as @e@ is /somewhere/
-- in the list.
--
-- For example, a computation that only needs access to a mutable value of type
-- 'Integer' would have the following type:
--
-- @
-- 'Effectful.State.Static.Local.State' 'Integer' ':>' es => 'Effectful.Eff' es ()
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

----------------------------------------

-- | Convenience operator for expressing that a function uses multiple effects
-- in a more concise way than enumerating them all with '(:>)'.
--
-- @[E1, E2, ..., En] ':>>' es ≡ (E1 ':>' es, E2 ':>' es, ..., En :> es)@
type family xs :>> es :: Constraint where
  '[]      :>> es = ()
  (x : xs) :>> es = (x :> es, xs :>> es)
