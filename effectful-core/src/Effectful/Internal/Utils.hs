{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Utils
  ( inlineBracket

    -- * Utils for 'ThreadId'
  , weakThreadId
  , eqThreadId

    -- * Utils for 'Any'
  , Any
  , toAny
  , fromAny

    -- * Unique
  , Unique
  , newUnique

  -- * CallStack
  , thawCallStack
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
#if __GLASGOW_HASKELL__ >= 904
import Data.Word
#else
import Foreign.C.Types
#endif
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
#if __GLASGOW_HASKELL__ >= 904
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6163
  rts_getThreadId :: Addr# -> Word64
#elif __GLASGOW_HASKELL__ >= 900
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1254
  rts_getThreadId :: Addr# -> CLong
#else
  rts_getThreadId :: Addr# -> CInt
#endif

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

#if __GLASGOW_HASKELL__ < 900

-- | 'Eq' instance for 'ThreadId' is broken in GHC < 9, see
-- https://gitlab.haskell.org/ghc/ghc/-/issues/16761 for more info.
eqThreadId :: ThreadId -> ThreadId -> Bool
eqThreadId (ThreadId t1#) (ThreadId t2#) =
  eq_thread (threadIdToAddr# t1#) (threadIdToAddr# t2#) == 1

foreign import ccall unsafe "effectful_eq_thread"
  eq_thread :: Addr# -> Addr# -> CLong

#else

eqThreadId :: ThreadId -> ThreadId -> Bool
eqThreadId = (==)

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
