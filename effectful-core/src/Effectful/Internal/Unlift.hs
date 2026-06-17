{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
  , ephemeralConcLimitedUnlift
  , ephemeralConcUnlimitedUnlift
  , persistentConcUnlift
  , persistentConcSingleUnlift
  , persistentConcUnlifts
  , persistentConcSingleUnlifts
  ) where

import Control.Concurrent
import Control.Concurrent.MVar.Strict
import Control.Monad
import Data.Coerce
import Data.Word
import GHC.Conc.Sync (ThreadId(..))
import GHC.Exts (mkWeak#, mkWeakNoFinalizer#)
import GHC.Generics (Generic)
import GHC.IO (IO(..))
import GHC.Stack (HasCallStack)
import GHC.Weak (Weak(..))
import System.Mem.Weak (deRefWeak)

import Effectful.Internal.Env
import Effectful.Internal.Utils
import Effectful.Internal.Utils.Word64Map qualified as M

----------------------------------------
-- Unlift strategies

-- | The strategy to use when unlifting 'Effectful.Eff' computations via
-- 'Effectful.withEffToIO' or the 'Effectful.Dispatch.Dynamic.localUnlift'
-- family.
data UnliftStrategy
  = SeqUnlift
  -- ^ The sequential strategy is the fastest and a default setting for
  -- t'Effectful.IOE'. Any attempt of calling the unlifting function in threads
  -- distinct from its creator will result in a runtime error.
  | SeqForkUnlift
  -- ^ Like 'SeqUnlift', but all unlifted actions will be executed in a cloned
  -- environment.
  --
  -- The main consequence is that thread local state is forked at the point of
  -- creation of the unlifting function and its modifications in unlifted
  -- actions will not affect the main thread of execution (and vice versa):
  --
  -- >>> import Effectful
  -- >>> import Effectful.State.Dynamic
  -- >>> :{
  --  action :: (IOE :> es, State Int :> es) => Eff es ()
  --  action = do
  --    modify @Int (+1)
  --    withEffToIO SeqForkUnlift $ \unlift -> unlift $ modify @Int (+2)
  --    modify @Int (+4)
  -- :}
  --
  -- >>> runEff . execStateLocal @Int 0 $ action
  -- 5
  --
  -- >>> runEff . execStateShared @Int 0 $ action
  -- 7
  --
  -- Because of this it's possible to safely use the unlifting function outside
  -- of the scope of effects it captures, e.g. by creating an @IO@ action that
  -- executes effectful operations and running it later:
  --
  -- >>> :{
  --   delayed :: UnliftStrategy -> IO (IO String)
  --   delayed strategy = runEff . evalStateLocal "Hey" $ do
  --     r <- withEffToIO strategy $ \unlift -> pure $ unlift get
  --     modify (++ "!!!")
  --     pure r
  -- :}
  --
  -- This doesn't work with the 'SeqUnlift' strategy because when the returned
  -- action runs, @State@ is no longer in scope:
  --
  -- >>> join $ delayed SeqUnlift
  -- *** Exception: version (...) /= storageVersion (0)
  -- ...
  --
  -- However, it does with the 'SeqForkUnlift' strategy:
  --
  -- >>> join $ delayed SeqForkUnlift
  -- "Hey"
  --
  | ConcUnlift !Persistence !Limit
  -- ^ The concurrent strategy makes it possible for the unlifting function to
  -- be called in threads distinct from its creator. See 'Persistence' and
  -- 'Limit' settings for more information.
  deriving stock (Eq, Generic, Ord, Show)

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
  deriving stock (Eq, Generic, Ord, Show)

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
  deriving stock (Eq, Generic, Ord, Show)

----------------------------------------
-- Unlift functions

-- | Concurrent unlift with limited uses that doesn't preserve the environment
-- between calls to the unlifting function in threads other than its creator.
--
-- @since 2.7.0.0
ephemeralConcLimitedUnlift
  :: (HasCallStack, forall r. Coercible (effEs r) (Env es -> IO r))
  => Env es
  -> Int
  -- ^ Number of permitted uses of the unlift function.
  -> ((forall r. effEs r -> IO r) -> IO a)
  -> IO a
ephemeralConcLimitedUnlift es0 uses k = do
  unless (uses > 0) $ do
    error $ "Invalid number of uses: " ++ show uses
  tid0 <- myThreadId
  -- Create a copy of the environment as a template for the other threads to
  -- use. This can't be done from inside the callback as the environment might
  -- have already changed by then.
  esTemplate <- cloneEnv es0
  mvUses <- newMVar' uses
  let getEs = myThreadId >>= \case
        tid | tid0 == tid -> pure es0
        _ -> modifyMVar' mvUses $ \case
          0 -> error
             $ "Number of permitted calls (" ++ show uses ++ ") to the unlifting "
            ++ "function in other threads was exceeded. Please increase the limit "
            ++ "or use the unlimited variant."
          1 -> pure (0, esTemplate)
          n -> do
            es <- cloneEnv esTemplate
            pure (n - 1, es)
  k $ \action -> coerce action =<< getEs
{-# INLINE ephemeralConcLimitedUnlift #-}

-- | Concurrent unlift with unlimited uses that doesn't preserve the environment
-- between calls to the unlifting function in threads other than its creator.
--
-- @since 2.7.0.0
ephemeralConcUnlimitedUnlift
  :: (HasCallStack, forall r. Coercible (effEs r) (Env es -> IO r))
  => Env es
  -> ((forall r. effEs r -> IO r) -> IO a)
  -> IO a
ephemeralConcUnlimitedUnlift es0 k = do
  tid0 <- myThreadId
  -- Create a copy of the environment as a template for the other threads to
  -- use. This can't be done from inside the callback as the environment might
  -- have already changed by then.
  esTemplate <- cloneEnv es0
  let getEs = myThreadId >>= \case
        tid | tid0 == tid -> pure es0
        _ -> cloneEnv esTemplate
  k $ \action -> coerce action =<< getEs
{-# INLINE ephemeralConcUnlimitedUnlift #-}

-- | Concurrent unlift that preserves the environment between calls to the
-- unlifting function within a particular thread.
persistentConcUnlift
  :: (HasCallStack, forall r. Coercible (effEs r) (Env es -> IO r))
  => Env es
  -> Bool
  -> Int
  -- ^ Number of threads that are allowed to use the unlift function.
  -> ((forall r. effEs r -> IO r) -> IO a)
  -> IO a
persistentConcUnlift es0 cleanUp threads k = do
  unless (threads > 0) $ do
    error $ "Invalid number of threads: " ++ show threads
  tid0 <- myThreadId
  -- Create a copy of the environment as a template for the other threads to
  -- use. This can't be done from inside the callback as the environment might
  -- have already changed by then.
  esTemplate <- cloneEnv es0
  mvEntries <- newMVar' $ ThreadEntries threads M.empty
  let getEs = myThreadId >>= \case
        tid | tid0 == tid -> pure es0
        tid -> do
          te0 <- readMVar' mvEntries
          let wkTid = weakThreadId tid
          case wkTid `M.lookup` te0.entries of
            Just wkEs -> getWkTidEnv wkEs
            -- If the environment is not in the map, there is no point checking
            -- again within modifyMVar' below, because this is the only thread
            -- that can put it there.
            Nothing -> modifyMVar' mvEntries $ \te -> case te.capacity of
              0 -> noCapacityError threads
              1 -> do
                wkTidEs <- mkWeakThreadIdEnv tid wkTid esTemplate mvEntries cleanUp
                let newEntries = ThreadEntries
                      { capacity = te.capacity - 1
                      , entries  = M.insert wkTid wkTidEs te.entries
                      }
                pure (newEntries, esTemplate)
              _ -> do
                es <- cloneEnv esTemplate
                wkTidEs <- mkWeakThreadIdEnv tid wkTid es mvEntries cleanUp
                let newEntries = ThreadEntries
                      { capacity = te.capacity - 1
                      , entries  = M.insert wkTid wkTidEs te.entries
                      }
                pure (newEntries, es)
  k $ \action -> coerce action =<< getEs
{-# INLINE persistentConcUnlift #-}

-- | Variant of 'persistentConcUnlift' for a single other thread that doesn't
-- need ThreadEntries.
--
-- @since 2.7.0.0
persistentConcSingleUnlift
  :: ( HasCallStack, forall r. Coercible (effEs r) (Env es -> IO r))
  => Env es
  -> ((forall r. effEs r -> IO r) -> IO a)
  -> IO a
persistentConcSingleUnlift es0 k = do
  tid0 <- myThreadId
  -- Create a copy of the environment for the other thread to use. This can't be
  -- done from inside the callback as the environment might have already changed
  -- by then.
  es <- cloneEnv es0
  -- GHC never labels threads as 0.
  mvWeakTid <- newMVar' 0
  let getEs = myThreadId >>= \case
        tid | tid0 == tid -> pure es0
        tid -> do
          let wkTid = weakThreadId tid
          readMVar' mvWeakTid >>= \case
            0 -> modifyMVar' mvWeakTid $ \case
              0 -> pure (wkTid, es)
              _ -> noCapacityError 1
            v | v == wkTid -> pure es
              | otherwise -> noCapacityError 1
  k $ \action -> coerce action =<< getEs
{-# INLINE persistentConcSingleUnlift #-}

-- | Variant of 'persistentConcUnlift' producing two unlifting functions that
-- share the effect storage in each thread.
--
-- @since 2.7.0.0
persistentConcUnlifts
  :: ( HasCallStack
     , forall r. Coercible (effEs r) (Env es -> IO r)
     , forall r. Coercible (effLocalEs r) (Env localEs -> IO r)
     )
  => Env es
  -> Env localEs
  -> Bool
  -> Int
  -- ^ Number of threads that are allowed to use the unlift function.
  -> ((forall r. effEs r -> IO r) -> (forall r. effLocalEs r -> IO r) -> IO a)
  -> IO a
persistentConcUnlifts es0 les0 cleanUp threads k = do
  unless (threads > 0) $ do
    error $ "Invalid number of threads: " ++ show threads
  tid0 <- myThreadId
  -- Create a copy of the environments sharing the effect storage as a template
  -- for the other threads to use. This can't be done from inside the callback
  -- as the environment might have already changed by then.
  storageTemplate <- cloneStorage es0.storage
  esTemplate <- replaceStorage es0 storageTemplate
  lesTemplate <- replaceStorage les0 storageTemplate
  mvEntries <- newMVar' $ ThreadEntries threads M.empty
  let getEsLes = myThreadId >>= \case
        tid | tid0 == tid -> pure (es0, les0)
        tid -> do
          te0 <- readMVar' mvEntries
          let wkTid = weakThreadId tid
          case wkTid `M.lookup` te0.entries of
            Just wkEsLes -> getWkTidEnv wkEsLes
            -- If the environments are not in the map, there is no point
            -- checking again within modifyMVar' below, because this is the only
            -- thread that can put them there.
            Nothing -> modifyMVar' mvEntries $ \te -> case te.capacity of
              0 -> noCapacityError threads
              1 -> do
                wkTidEsLes <- mkWeakThreadIdEnv tid wkTid (esTemplate, lesTemplate) mvEntries cleanUp
                let newEntries = ThreadEntries
                      { capacity = te.capacity - 1
                      , entries  = M.insert wkTid wkTidEsLes te.entries
                      }
                pure (newEntries, (esTemplate, lesTemplate))
              _ -> do
                storage <- cloneStorage storageTemplate
                es <- replaceStorage esTemplate storage
                les <- replaceStorage lesTemplate storage
                wkTidEsLes <- mkWeakThreadIdEnv tid wkTid (es, les) mvEntries cleanUp
                let newEntries = ThreadEntries
                      { capacity = te.capacity - 1
                      , entries  = M.insert wkTid wkTidEsLes te.entries
                      }
                pure (newEntries, (es, les))
  k (\action -> coerce action . fst =<< getEsLes)
    (\action -> coerce action . snd =<< getEsLes)
{-# INLINE persistentConcUnlifts #-}

-- | Variant of 'persistentConcUnlifts' for a single other thread that doesn't
-- need ThreadEntries.
--
-- @since 2.7.0.0
persistentConcSingleUnlifts
  :: ( HasCallStack
     , forall r. Coercible (effEs r) (Env es -> IO r)
     , forall r. Coercible (effLocalEs r) (Env localEs -> IO r)
     )
  => Env es
  -> Env localEs
  -> ((forall r. effEs r -> IO r) -> (forall r. effLocalEs r -> IO r) -> IO a)
  -> IO a
persistentConcSingleUnlifts es0 les0 k = do
  tid0 <- myThreadId
  -- Create a copy of the environments sharing the effect storage for the other
  -- thread to use. This can't be done from inside the callback as the
  -- environment might have already changed by then.
  storage <- cloneStorage es0.storage
  es <- replaceStorage es0 storage
  les <- replaceStorage les0 storage
  -- GHC never labels threads as 0.
  mvWeakTid <- newMVar' 0
  let getEsLes = myThreadId >>= \case
        tid | tid0 == tid -> pure (es0, les0)
        tid -> do
          let wkTid = weakThreadId tid
          readMVar' mvWeakTid >>= \case
            0 -> modifyMVar' mvWeakTid $ \case
              0 -> pure (wkTid, (es, les))
              _ -> noCapacityError 1
            v | v == wkTid -> pure (es, les)
              | otherwise -> noCapacityError 1
  k (\action -> coerce action . fst =<< getEsLes)
    (\action -> coerce action . snd =<< getEsLes)
{-# INLINE persistentConcSingleUnlifts #-}

----------------------------------------
-- Internal helpers

noCapacityError :: HasCallStack => Int -> a
noCapacityError threads = error
  $ "Number of other threads (" ++ show threads ++ ") permitted to "
  ++ "use the unlifting function was exceeded. Please increase the "
  ++ "limit or use the unlimited variant."

getWkTidEnv :: HasCallStack => Weak a -> IO a
getWkTidEnv wkTidEnv = deRefWeak wkTidEnv >>= \case
  Nothing -> error "Impossible, thread alive but its weak ref dead"
  Just env -> pure env

data ThreadEntries a = ThreadEntries
  { capacity :: !Int
  , entries  :: !(M.Word64Map (Weak a))
  }

mkWeakThreadIdEnv
  :: ThreadId
  -> Word64
  -> a
  -> MVar' (ThreadEntries a)
  -> Bool
  -> IO (Weak a)
mkWeakThreadIdEnv (ThreadId t#) wkTid es v = \case
  True -> IO $ \s0 ->
    case mkWeak# t# es finalizer s0 of
      (# s1, w #) -> (# s1, Weak w #)
  False -> IO $ \s0 ->
    case mkWeakNoFinalizer# t# es s0 of
      (# s1, w #) -> (# s1, Weak w #)
  where
    IO finalizer = modifyMVar'_ v $ \te -> do
      pure ThreadEntries
        { capacity = case te.capacity of
            -- If the template copy of the environment hasn't been consumed
            -- yet, the capacity can be restored.
            0 -> 0
            n -> n + 1
        , entries = M.delete wkTid te.entries
        }
