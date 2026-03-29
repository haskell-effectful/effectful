{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Utils
  ( inlineBracket

    -- * Utils for 'ThreadId'
  , weakThreadId

    -- * Utils for 'Any'
  , Any
  , toAny
  , fromAny

    -- * Unique
  , Unique
  , newUnique

    -- * CallStack
  , thawCallStack

    -- * Array capacity
  , growCapacity
  ) where

import Control.Exception
import Data.Primitive.ByteArray
import GHC.Conc.Sync (ThreadId(..))
import GHC.Exts (Any, RealWorld)
import GHC.Stack.Types (CallStack(..))
import Unsafe.Coerce (unsafeCoerce)

#if MIN_VERSION_base(4,19,0)
import GHC.Conc.Sync (fromThreadId)
#else
import GHC.Exts (Addr#, ThreadId#, unsafeCoerce#)
import Data.Word
#endif

-- | Version of bracket with an INLINE pragma to work around
-- https://gitlab.haskell.org/ghc/ghc/-/issues/22824.
inlineBracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
inlineBracket before after action = mask $ \unmask -> do
  a <- before
  r <- unmask (action a) `onException` after a
  _ <- after a
  pure r
{-# INLINE inlineBracket #-}

----------------------------------------

-- | Get an id of a thread that doesn't prevent its garbage collection.
weakThreadId :: ThreadId -> Int
#if MIN_VERSION_base(4,19,0)
weakThreadId = fromIntegral . fromThreadId
#else
weakThreadId (ThreadId t#) = fromIntegral $ rts_getThreadId (threadIdToAddr# t#)

foreign import ccall unsafe "rts_getThreadId"
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6163
  rts_getThreadId :: Addr# -> Word64

-- Note: FFI imports take Addr# instead of ThreadId# because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/8281, which would prevent loading
-- effectful-core into GHCi.
--
-- Previous workaround was to use an internal library with just this module, but
-- this is not viable because of bugs in stack (sigh).
--
-- The coercion is fine because GHC represents ThreadId# as a pointer.
threadIdToAddr# :: ThreadId# -> Addr#
threadIdToAddr# = unsafeCoerce#
#endif

----------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

----------------------------------------

-- | A unique with no possibility for CAS contention.
--
-- Credits for this go to Edward Kmett.
newtype Unique = Unique (MutableByteArray RealWorld)

instance Eq Unique where
  Unique a == Unique b = sameMutableByteArray a b

newUnique :: IO Unique
newUnique = Unique <$> newByteArray 0

----------------------------------------

thawCallStack :: CallStack -> CallStack
thawCallStack = \case
  FreezeCallStack cs -> cs
  cs -> cs

----------------------------------------

-- | Grow capacity of an array.
--
-- See https://archive.ph/Z2R8w.
growCapacity :: Int -> Int
growCapacity n = 1 + quot (n * 3) 2
