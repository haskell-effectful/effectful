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
  , Subset(..)
  , KnownPrefix(..)
  , IsUnknownSuffixOf

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
  ( Text "There is no handler for '" :<>: ShowType e :<>: Text "' in the context"
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
{-# DEPRECATED (:>>) "Usage of (:>>) slows down GHC too much, so it will be removed in 3.0.0.0. See https://github.com/haskell-effectful/effectful/issues/52#issuecomment-1269155485 for more information." #-}

----------------------------------------

-- | Provide evidence that @xs@ is a subset of @es@.
class KnownPrefix es => Subset (xs :: [Effect]) (es :: [Effect]) where
  reifyIndices :: [Int]
  reifyIndices = -- Don't show "minimal complete definition" in haddock.
                 error "unimplemented"

-- If the subset is not fully known, make sure the subset and the base stack
-- have the same unknown suffix.
instance {-# INCOHERENT #-}
  ( KnownPrefix es
  , xs `IsUnknownSuffixOf` es
  ) => Subset xs es where
  reifyIndices = []

-- If the subset is fully known, we're done.
instance KnownPrefix es => Subset '[] es where
  reifyIndices = []

instance (e :> es, Subset xs es) => Subset (e : xs) es where
  reifyIndices = reifyIndex @e @es : reifyIndices @xs @es

----

-- | Calculate length of a statically known prefix of @es@.
class KnownPrefix (es :: [Effect]) where
  prefixLength :: Int

instance KnownPrefix es => KnownPrefix (e : es) where
  prefixLength = 1 + prefixLength @es

instance {-# INCOHERENT #-} KnownPrefix es where
  prefixLength = 0

----

-- | Require that @xs@ is the unknown suffix of @es@.
class (xs :: [Effect]) `IsUnknownSuffixOf` (es :: [Effect])
instance {-# INCOHERENT #-} xs ~ es => xs `IsUnknownSuffixOf` es
instance xs `IsUnknownSuffixOf` es => xs `IsUnknownSuffixOf` (e : es)
