{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | Implementation of the concurrent unlift.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Unlift
  ( concUnliftIO
  ) where

import Control.Concurrent
import GHC.Conc.Sync (ThreadId(..))
import GHC.Exts (mkWeak#, mkWeakNoFinalizer#)
import GHC.IO (IO(..))
import GHC.Weak (Weak(..))
import System.Mem.Weak (deRefWeak)
import qualified Data.Hashable as H
import qualified Data.IntMap.Strict as IM

import Effectful.Internal.Env

concUnliftIO
  :: Bool
  -> Int
  -> ((forall r. m r -> IO r) -> IO a)
  -> Env es
  -> (forall r. m r -> Env es -> IO r)
  -> IO a
concUnliftIO cleanUp threads f es0 unEff = do
  tid0 <- myThreadId
  -- Create a copy of the environment as a template for the other threads to
  -- use. This can't be done from inside the callback as the environment might
  -- have already changed by then.
  esTemplate <- cloneEnv es0
  mvEntries <- newMVar $ ThreadEntries threads IM.empty
  f $ \m -> do
    es <- myThreadId >>= \case
      tid | tid0 == tid -> pure es0
      tid -> modifyMVar mvEntries $ \te -> do
        let hTid = H.hash tid
        (mes, i) <- case hTid `IM.lookup` teEntries te of
          Just (ThreadEntry i td) -> (, i) <$> lookupEnv tid td
          Nothing                 -> pure (Nothing, newEntryId)
        case mes of
          Just es -> pure (te, es)
          Nothing -> case teCapacity te of
            0 -> error $ "Number of allowed threads to run with the unlifting "
                      ++ "function exceeded. You have to increase the argument "
                      ++ "to BoundedConcUnlift or use UnboundedConcUnlift."
            1 -> do
              wkTidEs <- mkWeakThreadIdEnv tid esTemplate hTid i mvEntries cleanUp
              let newEntries = ThreadEntries
                    { teCapacity = teCapacity te - 1
                    , teEntries  = addThreadData hTid i wkTidEs $ teEntries te
                    }
              newEntries `seq` pure (newEntries, esTemplate)
            _ -> do
              es      <- cloneEnv esTemplate
              wkTidEs <- mkWeakThreadIdEnv tid es hTid i mvEntries cleanUp
              let newEntries = ThreadEntries
                    { teCapacity = teCapacity te - 1
                    , teEntries  = addThreadData hTid i wkTidEs $ teEntries te
                    }
              newEntries `seq` pure (newEntries, es)
    unEff m es

----------------------------------------
-- Data types

newtype EntryId = EntryId Int
  deriving Eq

newEntryId :: EntryId
newEntryId = EntryId 0

nextEntryId :: EntryId -> EntryId
nextEntryId (EntryId i) = EntryId (i + 1)

data ThreadEntries es = ThreadEntries
  { teCapacity :: Int
  , teEntries  :: IM.IntMap (ThreadEntry es)
  }

data ThreadEntry es = ThreadEntry EntryId (ThreadData es)

data ThreadData es
  = ThreadData EntryId (Weak (ThreadId, Env es)) (ThreadData es)
  | NoThreadData

----------------------------------------
-- Weak references to threads

mkWeakThreadIdEnv
  :: ThreadId
  -> Env es
  -> Int
  -> EntryId
  -> MVar (ThreadEntries es)
  -> Bool
  -> IO (Weak (ThreadId, Env es))
mkWeakThreadIdEnv t@(ThreadId t#) es h i v = \case
  True -> IO $ \s0 ->
    case mkWeak# t# (t, es) finalizer s0 of
      (# s1, w #) -> (# s1, Weak w #)
  False -> IO $ \s0 ->
    case mkWeakNoFinalizer# t# (t, es) s0 of
      (# s1, w #) -> (# s1, Weak w #)
  where
    IO finalizer = deleteThreadData h i v

----------------------------------------
-- Manipulation of ThreadEntries

lookupEnv :: ThreadId -> ThreadData es -> IO (Maybe (Env es))
lookupEnv tid0 = \case
  NoThreadData -> pure Nothing
  ThreadData _ wkTidEs td -> deRefWeak wkTidEs >>= \case
    Nothing -> lookupEnv tid0 td
    Just (tid, es)
      | tid0 == tid -> pure $ Just es
      | otherwise   -> lookupEnv tid0 td

----------------------------------------

addThreadData
  :: Int
  -> EntryId
  -> Weak (ThreadId, Env es)
  -> IM.IntMap (ThreadEntry es)
  -> IM.IntMap (ThreadEntry es)
addThreadData h i w teMap
  | i == newEntryId = IM.insert h (newThreadEntry i w) teMap
  | otherwise       = IM.adjust (consThreadData w) h teMap

newThreadEntry :: EntryId -> Weak (ThreadId, Env es) -> ThreadEntry es
newThreadEntry i w = ThreadEntry (nextEntryId i) $ ThreadData i w NoThreadData

consThreadData :: Weak (ThreadId, Env es) -> ThreadEntry es -> ThreadEntry es
consThreadData w (ThreadEntry i td) =
  ThreadEntry (nextEntryId i) $ ThreadData i w td

----------------------------------------

deleteThreadData :: Int -> EntryId -> MVar (ThreadEntries es) -> IO ()
deleteThreadData h i v = modifyMVar_ v $ \te -> do
  let newEntries = ThreadEntries
        { teCapacity = case teCapacity te of
            -- If the template copy of the environment hasn't been consumed
            -- yet, the capacity can be restored.
            0 -> 0
            n -> n + 1
        , teEntries = IM.update (cleanThreadEntry i) h $ teEntries te
        }
  newEntries `seq` pure newEntries

cleanThreadEntry :: EntryId -> ThreadEntry es -> Maybe (ThreadEntry es)
cleanThreadEntry i0 (ThreadEntry i td0) = case cleanThreadData i0 td0 of
  NoThreadData -> Nothing
  td           -> Just (ThreadEntry i td)

cleanThreadData :: EntryId -> ThreadData es -> ThreadData es
cleanThreadData i0 = \case
  NoThreadData -> NoThreadData
  ThreadData i w td
    | i0 == i   -> td
    | otherwise -> ThreadData i w (cleanThreadData i0 td)
