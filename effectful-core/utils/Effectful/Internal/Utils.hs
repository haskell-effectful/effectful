{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Utils
  ( weakThreadId
  , eqThreadId

  -- * Strict 'IORef'
  , IORef'
  , newIORef'
  , readIORef'
  , writeIORef'

  -- * Strict 'MVar'
  , MVar'
  , toMVar'
  , newMVar'
  , readMVar'
  , modifyMVar'
  , modifyMVar_'

  -- * Utils for 'Any'
  , Any
  , toAny
  , fromAny
  ) where

import Control.Concurrent.MVar
import Data.IORef
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

-- | A strict variant of 'IORef'.
newtype IORef' a = IORef' (IORef a)

newIORef' :: a -> IO (IORef' a)
newIORef' a = a `seq` (IORef' <$> newIORef a)

readIORef' :: IORef' a -> IO a
readIORef' (IORef' var) = readIORef var

writeIORef' :: IORef' a -> a -> IO ()
writeIORef' (IORef' var) a = a `seq` writeIORef var a

----------------------------------------

-- | A strict variant of 'MVar'.
newtype MVar' a = MVar' (MVar a)

toMVar' :: MVar a -> IO (MVar' a)
toMVar' var = do
  let var' = MVar' var
  modifyMVar_' var' pure
  pure var'

newMVar' :: a -> IO (MVar' a)
newMVar' a = a `seq` (MVar' <$> newMVar a)

readMVar' :: MVar' a -> IO a
readMVar' (MVar' var) = readMVar var

modifyMVar' :: MVar' a -> (a -> IO (a, r)) -> IO r
modifyMVar' (MVar' var) action = modifyMVar var $ \a0 -> do
  (a, r) <- action a0
  a `seq` pure (a, r)
{-# INLINE modifyMVar' #-}

modifyMVar_' :: MVar' a -> (a -> IO a) -> IO ()
modifyMVar_' (MVar' var) action = modifyMVar_ var $ \a0 -> do
  a <- action a0
  a `seq` pure a
{-# INLINE modifyMVar_' #-}

----------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce
