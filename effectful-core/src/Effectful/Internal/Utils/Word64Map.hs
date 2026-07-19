{-# LANGUAGE Strict #-}
-- | A minimal, strict map keyed by 'Word64' values (adaptation of
-- 'Data.IntMap.Strict').
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Utils.Word64Map
  ( Word64Map
  , empty
  , lookup
  , insert
  , delete
  , updateLookupWithKey
  ) where

import Data.Bits
import Data.Word
import Prelude hiding (lookup)

-- | A map of 'Word64' keys to values of type @a@.
data Word64Map a
  = Bin Prefix (Word64Map a) (Word64Map a)
  | Tip Word64 a
  | Nil

-- | A @Prefix@ represents some prefix of high-order bits of a @Word64@.
newtype Prefix = Prefix Word64

unPrefix :: Prefix -> Word64
unPrefix (Prefix p) = p

----------------------------------------

-- | The empty map.
empty :: Word64Map a
empty = Nil

-- | Look up the value at a key in the map.
lookup :: Word64 -> Word64Map a -> Maybe a
lookup k = go
  where
    go (Bin p l r) | left k p  = go l
                   | otherwise = go r
    go (Tip kx x) | k == kx   = Just x
                  | otherwise = Nothing
    go Nil = Nothing

-- | Insert a new key/value pair in the map. If the key is already present, the
-- associated value is replaced with the supplied one.
--
-- The value is evaluated to WHNF when it is inserted into the map.
insert :: Word64 -> a -> Word64Map a -> Word64Map a
insert k x = go
  where
    go t@(Bin p l r)
      | nomatch k p = linkKey k (Tip k x) p t
      | left k p    = Bin p (go l) r
      | otherwise   = Bin p l (go r)
    go t@(Tip ky _)
      | k == ky     = Tip k x
      | otherwise   = link k (Tip k x) ky t
    go Nil = Tip k x

-- | Delete a key and its value from the map. When the key is not a member of
-- the map, the original map is returned.
delete :: Word64 -> Word64Map a -> Word64Map a
delete k = go
  where
    go t@(Bin p l r)
      | nomatch k p = t
      | left k p    = binCheckLeft p (go l) r
      | otherwise   = binCheckRight p l (go r)
    go t@(Tip ky _)
      | k == ky     = Nil
      | otherwise   = t
    go Nil = Nil

-- | Look up and update the value at a key in the map. The function returns the
-- original value, if it exists, and the updated map.
--
-- The updated value is evaluated to WHNF when it is inserted into the map.
updateLookupWithKey
  :: (Word64 -> a -> Maybe a)
  -> Word64
  -> Word64Map a
  -> (Maybe a, Word64Map a)
updateLookupWithKey f k = go
  where
    go t@(Bin p l r)
      | nomatch k p = (Nothing, t)
      | left k p    = let (found, l') = go l in (found, binCheckLeft p l' r)
      | otherwise   = let (found, r') = go r in (found, binCheckRight p l r')
    go t@(Tip ky y)
      | k == ky     = case f ky y of
          Just y' -> (Just y, Tip ky y')
          Nothing -> (Just y, Nil)
      | otherwise   = (Nothing, t)
    go Nil = (Nothing, Nil)

----------------------------------------
-- Internal helpers

-- | Whether the @Word64@ does not start with the given @Prefix@.
--
-- A @Word64@ starts with a @Prefix@ if it shares the high bits with the
-- internal @Word64@ value of the @Prefix@ up to the mask bit.
--
-- @nomatch@ is usually used to determine whether a key belongs in a @Bin@,
-- since all keys in a @Bin@ share a @Prefix@.
nomatch :: Word64 -> Prefix -> Bool
nomatch i (Prefix p) = (i `xor` p) .&. prefixMask /= 0
  where
    prefixMask = p `xor` (-p)

-- | Whether the @Word64@ is to the left of the split created by a @Bin@ with
-- this @Prefix@.
--
-- This does not imply that the @Word64@ belongs in this @Bin@. That fact is
-- usually determined first using @nomatch@.
left :: Word64 -> Prefix -> Bool
left i p = i < unPrefix p

-- | Link two @Word64Map@s. The maps must not be empty. The @Prefix@es of the
-- two maps must be different. @k1@ must share the prefix of @t1@. @p2@ must be
-- the prefix of @t2@.
linkKey :: Word64 -> Word64Map a -> Prefix -> Word64Map a -> Word64Map a
linkKey k1 t1 p2 t2 = link k1 t1 (unPrefix p2) t2

-- | Link two @Word64Map@s. The maps must not be empty. The @Prefix@es of the
-- two maps must be different. @k1@ must share the prefix of @t1@ and @k2@ must
-- share the prefix of @t2@.
link :: Word64 -> Word64Map a -> Word64 -> Word64Map a -> Word64Map a
link k1 t1 k2 t2 = linkWithMask (branchMask k1 k2) k1 t1 k2 t2

-- `linkWithMask` is useful when the `branchMask` has already been computed
linkWithMask :: Word64 -> Word64 -> Word64Map a -> Word64 -> Word64Map a -> Word64Map a
linkWithMask m k1 t1 k2 t2
  | k1 < k2   = Bin p t1 t2
  | otherwise = Bin p t2 t1
  where
    p = Prefix (mask k1 m .|. m)

-- | The prefix of key @i@ up to (but not including) the switching bit @m@.
mask :: Word64 -> Word64 -> Word64
mask i m = i .&. (m `xor` (-m))

-- | The first switching bit where the two prefixes disagree.
--
-- Precondition for defined behavior: p1 /= p2.
branchMask :: Word64 -> Word64 -> Word64
branchMask k1 k2 =
  unsafeShiftL 1 (finiteBitSize (0 :: Word64) - 1 - countLeadingZeros (k1 `xor` k2))

-- | Smart constructor that collapses an empty left subtree.
binCheckLeft :: Prefix -> Word64Map a -> Word64Map a -> Word64Map a
binCheckLeft _ Nil r = r
binCheckLeft p l   r = Bin p l r

-- | Smart constructor that collapses an empty right subtree.
binCheckRight :: Prefix -> Word64Map a -> Word64Map a -> Word64Map a
binCheckRight _ l Nil = l
binCheckRight p l   r = Bin p l r
