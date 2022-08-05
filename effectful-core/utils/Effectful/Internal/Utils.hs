{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Utils
  ( weakThreadId
  , eqThreadId

  -- * Utils for 'Any'
  , Any
  , toAny
  , fromAny
  ) where

import Foreign.C.Types
import GHC.Conc.Sync (ThreadId(..))
import GHC.Exts (Any, ThreadId#)
import Unsafe.Coerce (unsafeCoerce)

-- | Get an id of a thread that doesn't prevent its garbage collection.
weakThreadId :: ThreadId -> Int
weakThreadId (ThreadId t#) = fromIntegral $ rts_getThreadId t#

foreign import ccall unsafe "rts_getThreadId"
#if __GLASGOW_HASKELL__ >= 904
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6163
  rts_getThreadId :: ThreadId# -> CULLong
#elif __GLASGOW_HASKELL__ >= 900
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1254
  rts_getThreadId :: ThreadId# -> CLong
#else
  rts_getThreadId :: ThreadId# -> CInt
#endif

-- | 'Eq' instance for 'ThreadId' is broken in GHC < 9, see
-- https://gitlab.haskell.org/ghc/ghc/-/issues/16761 for more info.
eqThreadId :: ThreadId -> ThreadId -> Bool
eqThreadId (ThreadId t1#) (ThreadId t2#) = eq_thread t1# t2# == 1

foreign import ccall unsafe "effectful_eq_thread"
  eq_thread :: ThreadId# -> ThreadId# -> CLong

----------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce
