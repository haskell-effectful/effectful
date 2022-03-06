{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | Implementation of sequential and concurrent unlifts.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Unlift
  ( -- * Unlifting strategies
    UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)

    -- * Unlifting functions
  , seqUnlift
  , concUnlift
  , ephemeralConcUnlift
  , persistentConcUnlift
  ) where

import Control.Concurrent
import Control.Monad
import GHC.Conc.Sync (ThreadId(..))
import GHC.Exts (mkWeak#, mkWeakNoFinalizer#)
import GHC.Generics (Generic)
import GHC.IO (IO(..))
import GHC.Stack (HasCallStack)
import GHC.Weak (Weak(..))
import System.Mem.Weak (deRefWeak)
import qualified Data.IntMap.Strict as IM

import Effectful.Internal.Env
import Effectful.Internal.Utils

----------------------------------------
-- Unlift strategies

-- | The strategy to use when unlifting 'Effectful.Eff' computations via
-- 'Effectful.withEffToIO', 'Effectful.withRunInIO' or the
-- 'Effectful.Dispatch.Dynamic.localUnlift' family.
data UnliftStrategy
  = SeqUnlift
  -- ^ The fastest strategy and a default setting for 'IOE'. An attempt to call
  -- the unlifting function in threads distinct from its creator will result in
  -- a runtime error.
  | ConcUnlift !Persistence !Limit
  -- ^ A strategy that makes it possible for the unlifting function to be called
  -- in threads distinct from its creator. See 'Persistence' and 'Limit'
  -- settings for more information.
  deriving (Eq, Generic, Ord, Show)

-- | Persistence setting for the 'ConcUnlift' strategy.
--
-- Different functions require different persistence strategies. Examples:
--
-- - Lifting 'pooledMapConcurrentlyN' from the @unliftio@ library requires the
--   'Ephemeral' strategy as we don't want jobs to share environment changes
--   made by previous jobs run in the same worker thread.
--
-- - Lifting 'Control.Concurrent.forkIOWithUnmask' requires the 'Persistent'
--   strategy, otherwise the unmasking function would start with a fresh
--   environment each time it's called.
data Persistence
  = Ephemeral
  -- ^ Don't persist the environment between calls to the unlifting function in
  -- threads distinct from its creator.
  | Persistent
  -- ^ Persist the environment between calls to the unlifting function within a
  -- particular thread.
  deriving (Eq, Generic, Ord, Show)

-- | Limit setting for the 'ConcUnlift' strategy.
data Limit
  = Limited !Int
  -- ^ Behavior dependent on the 'Persistence' setting.
  --
  -- For 'Ephemeral', it limits the amount of uses of the unlifting function in
  -- threads distinct from its creator to @N@. The unlifting function will
  -- create @N@ copies of the environment when called @N@ times and @K+1@ copies
  -- when called @K < N@ times.
  --
  -- For 'Persistent', it limits the amount of threads, distinct from the
  -- creator of the unlifting function, it can be called in to @N@. The amount
  -- of calls to the unlifting function within a particular threads is
  -- unlimited. The unlifting function will create @N@ copies of the environment
  -- when called in @N@ threads and @K+1@ copies when called in @K < N@ threads.
  | Unlimited
  -- ^ Unlimited use of the unlifting function.
  deriving (Eq, Generic, Ord, Show)

----------------------------------------
-- Unlift functions

-- | Sequential unlift.
seqUnlift
  :: HasCallStack
  => ((forall r. m r -> IO r) -> IO a)
  -> Env es
  -> (forall r. m r -> Env es -> IO r)
  -> IO a
seqUnlift k es unEff = do
  tid0 <- myThreadId
  k $ \m -> do
    tid <- myThreadId
    if tid `eqThreadId` tid0
      then unEff m es
      else error
         $ "If you want to use the unlifting function to run Eff computations "
        ++ "in multiple threads, have a look at UnliftStrategy (ConcUnlift)."

-- | Concurrent unlift for various strategies and limits.
concUnlift
  :: HasCallStack
  => Persistence
  -> Limit
  -> ((forall r. m r -> IO r) -> IO a)
  -> Env es
  -> (forall r. m r -> Env es -> IO r)
  -> IO a
concUnlift Ephemeral (Limited uses) k =
  ephemeralConcUnlift uses k
concUnlift Ephemeral Unlimited k =
  ephemeralConcUnlift maxBound k
concUnlift Persistent (Limited threads) k =
  persistentConcUnlift False threads k
concUnlift Persistent Unlimited k =
  persistentConcUnlift True maxBound k

-- | Concurrent unlift that doesn't preserve the environment between calls to
-- the unlifting function in threads other than its creator.
ephemeralConcUnlift
  :: HasCallStack
  => Int
  -- ^ Number of permitted uses of the unlift function.
  -> ((forall r. m r -> IO r) -> IO a)
  -> Env es
  -> (forall r. m r -> Env es -> IO r)
  -> IO a
ephemeralConcUnlift uses k es0 unEff = do
  unless (uses > 0) $ do
    error $ "Invalid number of uses: " ++ show uses
  tid0 <- myThreadId
  -- Create a copy of the environment as a template for the other threads to
  -- use. This can't be done from inside the callback as the environment might
  -- have already changed by then.
  esTemplate <- cloneEnv es0
  mvUses <- newMVar uses
  k $ \m -> do
    es <- myThreadId >>= \case
      tid | tid0 `eqThreadId` tid -> pure es0
      _ -> modifyMVar mvUses $ \case
        0 -> error
           $ "Number of permitted calls (" ++ show uses ++ ") to the unlifting "
          ++ "function in other threads was exceeded. Please increase the limit "
          ++ "or use the unlimited variant."
        1 -> pure (0, esTemplate)
        n -> do
          let newUses = n - 1
          es <- cloneEnv esTemplate
          newUses `seq` pure (newUses, es)
    unEff m es

-- | Concurrent unlift that preserves the environment between calls to the
-- unlifting function within a particular thread.
persistentConcUnlift
  :: HasCallStack
  => Bool
  -> Int
  -- ^ Number of threads that are allowed to use the unlift function.
  -> ((forall r. m r -> IO r) -> IO a)
  -> Env es
  -> (forall r. m r -> Env es -> IO r)
  -> IO a
persistentConcUnlift cleanUp threads k es0 unEff = do
  unless (threads > 0) $ do
    error $ "Invalid number of threads: " ++ show threads
  tid0 <- myThreadId
  -- Create a copy of the environment as a template for the other threads to
  -- use. This can't be done from inside the callback as the environment might
  -- have already changed by then.
  esTemplate <- cloneEnv es0
  mvEntries <- newMVar $ ThreadEntries threads IM.empty
  k $ \m -> do
    es <- myThreadId >>= \case
      tid | tid0 `eqThreadId` tid -> pure es0
      tid -> modifyMVar mvEntries $ \te -> do
        let wkTid = weakThreadId tid
        (mes, i) <- case wkTid `IM.lookup` teEntries te of
          Just (ThreadEntry i td) -> (, i) <$> lookupEnv tid td
          Nothing                 -> pure (Nothing, newEntryId)
        case mes of
          Just es -> pure (te, es)
          Nothing -> case teCapacity te of
            0 -> error
              $ "Number of other threads (" ++ show threads ++ ") permitted to "
              ++ "use the unlifting function was exceeded. Please increase the "
              ++ "limit or use the unlimited variant."
            1 -> do
              wkTidEs <- mkWeakThreadIdEnv tid esTemplate wkTid i mvEntries cleanUp
              let newEntries = ThreadEntries
                    { teCapacity = teCapacity te - 1
                    , teEntries  = addThreadData wkTid i wkTidEs $ teEntries te
                    }
              newEntries `seq` pure (newEntries, esTemplate)
            _ -> do
              es      <- cloneEnv esTemplate
              wkTidEs <- mkWeakThreadIdEnv tid es wkTid i mvEntries cleanUp
              let newEntries = ThreadEntries
                    { teCapacity = teCapacity te - 1
                    , teEntries  = addThreadData wkTid i wkTidEs $ teEntries te
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
  { teCapacity :: !Int
  , teEntries  :: !(IM.IntMap (ThreadEntry es))
  }

-- | In GHC < 9 weak thread ids are 32bit long, while ThreadIdS are 64bit long,
-- so there is potential for collisions. This is solved by keeping, for a
-- particular weak thread id, a list of ThreadIdS with unique EntryIdS.
data ThreadEntry es = ThreadEntry !EntryId !(ThreadData es)

data ThreadData es
  = ThreadData !EntryId !(Weak (ThreadId, Env es)) (ThreadData es)
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
mkWeakThreadIdEnv t@(ThreadId t#) es wkTid i v = \case
  True -> IO $ \s0 ->
    case mkWeak# t# (t, es) finalizer s0 of
      (# s1, w #) -> (# s1, Weak w #)
  False -> IO $ \s0 ->
    case mkWeakNoFinalizer# t# (t, es) s0 of
      (# s1, w #) -> (# s1, Weak w #)
  where
    IO finalizer = deleteThreadData wkTid i v

----------------------------------------
-- Manipulation of ThreadEntries

lookupEnv :: ThreadId -> ThreadData es -> IO (Maybe (Env es))
lookupEnv tid0 = \case
  NoThreadData -> pure Nothing
  ThreadData _ wkTidEs td -> deRefWeak wkTidEs >>= \case
    Nothing -> lookupEnv tid0 td
    Just (tid, es)
      | tid0 `eqThreadId` tid -> pure $ Just es
      | otherwise             -> lookupEnv tid0 td

----------------------------------------

addThreadData
  :: Int
  -> EntryId
  -> Weak (ThreadId, Env es)
  -> IM.IntMap (ThreadEntry es)
  -> IM.IntMap (ThreadEntry es)
addThreadData wkTid i w teMap
  | i == newEntryId = IM.insert wkTid (newThreadEntry i w) teMap
  | otherwise       = IM.adjust (consThreadData w) wkTid teMap

newThreadEntry :: EntryId -> Weak (ThreadId, Env es) -> ThreadEntry es
newThreadEntry i w = ThreadEntry (nextEntryId i) $ ThreadData i w NoThreadData

consThreadData :: Weak (ThreadId, Env es) -> ThreadEntry es -> ThreadEntry es
consThreadData w (ThreadEntry i td) =
  ThreadEntry (nextEntryId i) $ ThreadData i w td

----------------------------------------

deleteThreadData :: Int -> EntryId -> MVar (ThreadEntries es) -> IO ()
deleteThreadData wkTid i v = modifyMVar_ v $ \te -> do
  let newEntries = ThreadEntries
        { teCapacity = case teCapacity te of
            -- If the template copy of the environment hasn't been consumed
            -- yet, the capacity can be restored.
            0 -> 0
            n -> n + 1
        , teEntries = IM.update (cleanThreadEntry i) wkTid $ teEntries te
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
