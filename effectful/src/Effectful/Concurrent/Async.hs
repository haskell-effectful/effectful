-- | Lifted "Control.Concurrent.Async".
module Effectful.Concurrent.Async
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * Asynchronous actions
  , Async

    -- * High-level API

    -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask
  , withAsyncOnWithUnmask

    -- ** Querying 'Async's
  , wait, poll, waitCatch, A.asyncThreadId
  , cancel, cancelMany, uninterruptibleCancel, cancelWith
  , A.AsyncCancelled(..)
  , A.compareAsyncs

    -- ** High-level utilities
  , race, race_
  , concurrently, concurrently_
  , mapConcurrently, forConcurrently
  , mapConcurrently_, forConcurrently_
  , replicateConcurrently, replicateConcurrently_

    -- *** Concurrently
  , Concurrently(..)

    -- *** Conc
  , Conc, conc, runConc, U.ConcException(..)

    -- ** Pooled concurrency
  , pooledMapConcurrentlyN
  , pooledMapConcurrently
  , pooledMapConcurrentlyN_
  , pooledMapConcurrently_
  , pooledForConcurrentlyN
  , pooledForConcurrently
  , pooledForConcurrentlyN_
  , pooledForConcurrently_
  , pooledReplicateConcurrentlyN
  , pooledReplicateConcurrently
  , pooledReplicateConcurrentlyN_
  , pooledReplicateConcurrently_

    -- ** Specialised operations

    -- *** STM operations
  , A.waitSTM, A.pollSTM, A.waitCatchSTM

    -- *** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , waitEither_
  , waitBoth

    -- *** Waiting for multiple 'Async's in STM
  , A.waitAnySTM, A.waitAnyCatchSTM
  , A.waitEitherSTM, A.waitEitherCatchSTM
  , A.waitEitherSTM_
  , A.waitBothSTM

    -- * Low-level API

    -- ** Spawning (low-level API)
  , async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask

    -- ** Linking
  , link, linkOnly, link2, link2Only, A.ExceptionInLinkedThread(..)
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Exception (Exception, SomeException)
import Control.Monad (forever)
import Data.Kind (Type)
import Control.Concurrent.Async qualified as A
import UnliftIO.Async qualified as U
import UnliftIO.Internals.Async qualified as I

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Dispatch.Static.Unsafe

-- | Lifted 'Control.Concurrent.Async.async'.
async :: (HasCallStack, Concurrent :> es) => Eff es a -> Eff es (Async a)
async = liftAsync A.async

-- | Lifted 'Control.Concurrent.Async.asyncBound'.
asyncBound :: (HasCallStack, Concurrent :> es) => Eff es a -> Eff es (Async a)
asyncBound = liftAsync A.asyncBound

-- | Lifted 'Control.Concurrent.Async.asyncOn'.
asyncOn :: (HasCallStack, Concurrent :> es) => Int -> Eff es a -> Eff es (Async a)
asyncOn cpu = liftAsync (A.asyncOn cpu)

-- | Lifted 'Control.Concurrent.Async.asyncWithUnmask'.
asyncWithUnmask
  :: (HasCallStack, Concurrent :> es)
  => ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
asyncWithUnmask = liftAsyncWithUnmask A.asyncWithUnmask

-- | Lifted 'Control.Concurrent.Async.asyncOnWithUnmask'.
asyncOnWithUnmask
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
asyncOnWithUnmask cpu = liftAsyncWithUnmask (A.asyncOnWithUnmask cpu)

-- | Lifted 'Control.Concurrent.Async.withAsync'.
withAsync
  :: (HasCallStack, Concurrent :> es)
  => Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsync = liftWithAsync A.withAsync

-- | Lifted 'Control.Concurrent.Async.withAsyncBound'.
withAsyncBound
  :: (HasCallStack, Concurrent :> es)
  => Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncBound = liftWithAsync A.withAsyncBound

-- | Lifted 'Control.Concurrent.Async.withAsyncOn'.
withAsyncOn
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncOn cpu = liftWithAsync (A.withAsyncOn cpu)

-- | Lifted 'Control.Concurrent.Async.withAsyncWithUnmask'.
withAsyncWithUnmask
  :: (HasCallStack, Concurrent :> es)
  => ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncWithUnmask = liftWithAsyncWithUnmask A.withAsyncWithUnmask

-- | Lifted 'Control.Concurrent.Async.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncOnWithUnmask cpu = liftWithAsyncWithUnmask (A.withAsyncOnWithUnmask cpu)

-- | Lifted 'Control.Concurrent.Async.wait'.
wait :: Concurrent :> es => Async a -> Eff es a
wait = unsafeEff_ . A.wait

-- | Lifted 'Control.Concurrent.Async.poll'.
poll
  :: Concurrent :> es
  => Async a
  -> Eff es (Maybe (Either SomeException a))
poll = unsafeEff_ . A.poll

-- | Lifted 'Control.Concurrent.Async.cancel'.
cancel :: Concurrent :> es => Async a -> Eff es ()
cancel = unsafeEff_ . A.cancel

-- | Lifted 'Control.Concurrent.Async.cancelMany'.
--
-- @since 2.6.1.0
cancelMany :: Concurrent :> es => [Async a] -> Eff es ()
cancelMany = unsafeEff_ . A.cancelMany

-- | Lifted 'Control.Concurrent.Async.cancelWith'.
cancelWith :: (Exception e, Concurrent :> es) => Async a -> e -> Eff es ()
cancelWith a = unsafeEff_ . A.cancelWith a

-- | Lifted 'Control.Concurrent.Async.uninterruptibleCancel'.
uninterruptibleCancel :: Concurrent :> es => Async a -> Eff es ()
uninterruptibleCancel = unsafeEff_ . A.uninterruptibleCancel

-- | Lifted 'Control.Concurrent.Async.waitCatch'.
waitCatch
  :: Concurrent :> es
  => Async a
  -> Eff es (Either SomeException a)
waitCatch = unsafeEff_ . A.waitCatch

-- | Lifted 'Control.Concurrent.Async.waitAny'.
waitAny :: Concurrent :> es => [Async a] -> Eff es (Async a, a)
waitAny = unsafeEff_ . A.waitAny

-- | Lifted 'Control.Concurrent.Async.waitAnyCatch'.
waitAnyCatch
  :: Concurrent :> es
  => [Async a]
  -> Eff es (Async a, Either SomeException a)
waitAnyCatch = unsafeEff_ . A.waitAnyCatch

-- | Lifted 'Control.Concurrent.Async.waitAnyCancel'.
waitAnyCancel :: Concurrent :> es => [Async a] -> Eff es (Async a, a)
waitAnyCancel = unsafeEff_ . A.waitAnyCancel

-- | Lifted 'Control.Concurrent.Async.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: Concurrent :> es
  => [Async a]
  -> Eff es (Async a, Either SomeException a)
waitAnyCatchCancel = unsafeEff_ . A.waitAnyCatchCancel

-- | Lifted 'Control.Concurrent.Async.waitEither'.
waitEither
  :: Concurrent :> es
  => Async a
  -> Async b
  -> Eff es (Either a b)
waitEither a b = unsafeEff_ $ A.waitEither a b

-- | Lifted 'Control.Concurrent.Async.waitEitherCatch'.
waitEitherCatch
  :: Concurrent :> es
  => Async a
  -> Async b
  -> Eff es (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b = unsafeEff_ $ A.waitEitherCatch a b

-- | Lifted 'Control.Concurrent.Async.waitEitherCancel'.
waitEitherCancel
  :: Concurrent :> es
  => Async a
  -> Async b
  -> Eff es (Either a b)
waitEitherCancel a b = unsafeEff_ $ A.waitEitherCancel a b

-- | Lifted 'Control.Concurrent.Async.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: Concurrent :> es
  => Async a
  -> Async b
  -> Eff es (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b = unsafeEff_ $ A.waitEitherCatchCancel a b

-- | Lifted 'Control.Concurrent.Async.waitEither_'.
waitEither_ :: Concurrent :> es => Async a -> Async b -> Eff es ()
waitEither_ a b = unsafeEff_ $ A.waitEither_ a b

-- | Lifted 'Control.Concurrent.Async.waitBoth'.
waitBoth :: Concurrent :> es => Async a -> Async b -> Eff es (a, b)
waitBoth a b = unsafeEff_ $ A.waitBoth a b

-- | Lifted 'Control.Concurrent.Async.link'.
link :: Concurrent :> es => Async a -> Eff es ()
link = unsafeEff_ . A.link

-- | Lifted 'Control.Concurrent.Async.linkOnly'.
linkOnly :: Concurrent :> es => (SomeException -> Bool) -> Async a -> Eff es ()
linkOnly f = unsafeEff_ . A.linkOnly f

-- | Lifted 'Control.Concurrent.Async.link2'.
link2 :: Concurrent :> es => Async a -> Async b -> Eff es ()
link2 a b = unsafeEff_ $ A.link2 a b

-- | Lifted 'Control.Concurrent.Async.link2Only'.
link2Only :: Concurrent :> es => (SomeException -> Bool) -> Async a -> Async b -> Eff es ()
link2Only f a b = unsafeEff_ $ A.link2Only f a b

-- | Lifted 'Control.Concurrent.Async.race'.
race :: (HasCallStack, Concurrent :> es) => Eff es a -> Eff es b -> Eff es (Either a b)
race ma mb = unsafeEff $ \es -> do
  A.race (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- | Lifted 'Control.Concurrent.Async.race_'.
race_ :: (HasCallStack, Concurrent :> es) => Eff es a -> Eff es b -> Eff es ()
race_ ma mb = unsafeEff $ \es -> do
  A.race_ (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- | Lifted 'Control.Concurrent.Async.concurrently'.
concurrently :: (HasCallStack, Concurrent :> es) => Eff es a -> Eff es b -> Eff es (a, b)
concurrently ma mb = unsafeEff $ \es -> do
  A.concurrently (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- | Lifted 'Control.Concurrent.Async.concurrently_'.
concurrently_ :: (HasCallStack, Concurrent :> es) => Eff es a -> Eff es b -> Eff es ()
concurrently_ ma mb = unsafeEff $ \es -> do
  A.concurrently_ (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- Below functions use variants from the @unliftio@ library as they minimize the
-- amount of spawned threads and are thus much more efficient than the ones from
-- the @async@ library.

-- | Lifted 'Control.Concurrent.Async.mapConcurrently'.
mapConcurrently
  :: (HasCallStack, Traversable f, Concurrent :> es)
  => (a -> Eff es b)
  -> f a
  -> Eff es (f b)
mapConcurrently f t = unsafeEff $ \es -> do
  U.mapConcurrently (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'Control.Concurrent.Async.mapConcurrently_'.
mapConcurrently_
  :: (HasCallStack, Foldable f, Concurrent :> es)
  => (a -> Eff es b)
  -> f a
  -> Eff es ()
mapConcurrently_ f t = unsafeEff $ \es -> do
  U.mapConcurrently_ (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'Control.Concurrent.Async.forConcurrently'.
forConcurrently
  :: (HasCallStack, Traversable f, Concurrent :> es)
  => f a
  -> (a -> Eff es b)
  -> Eff es (f b)
forConcurrently t f = unsafeEff $ \es -> do
  U.forConcurrently t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'Control.Concurrent.Async.forConcurrently_'.
forConcurrently_
  :: (HasCallStack, Foldable f, Concurrent :> es)
  => f a
  -> (a -> Eff es b)
  -> Eff es ()
forConcurrently_ t f = unsafeEff $ \es -> do
  U.forConcurrently_ t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'Control.Concurrent.Async.replicateConcurrently'.
replicateConcurrently
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Eff es a
  -> Eff es [a]
replicateConcurrently n f = unsafeEff $ \es -> do
  U.replicateConcurrently n (unEff f =<< cloneEnv es)

-- | Lifted 'Control.Concurrent.Async.replicateConcurrently_'.
replicateConcurrently_
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Eff es a
  -> Eff es ()
replicateConcurrently_ n f = unsafeEff $ \es -> do
  U.replicateConcurrently_ n (unEff f =<< cloneEnv es)

----------------------------------------
-- Pooled concurrency (unliftio)

-- | Lifted 'UnliftIO.Async.pooledMapConcurrentlyN'.
pooledMapConcurrentlyN
  :: (HasCallStack, Concurrent :> es, Traversable t)
  => Int
  -> (a -> Eff es b)
  -> t a
  -> Eff es (t b)
pooledMapConcurrentlyN  threads f t = unsafeEff $ \es -> do
  U.pooledMapConcurrentlyN threads (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'UnliftIO.Async.pooledMapConcurrently'.
pooledMapConcurrently
  :: (HasCallStack, Concurrent :> es, Traversable t)
  => (a -> Eff es b)
  -> t a
  -> Eff es (t b)
pooledMapConcurrently f t = unsafeEff $ \es -> do
  U.pooledMapConcurrently (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'UnliftIO.Async.pooledMapConcurrentlyN_'.
pooledMapConcurrentlyN_
  :: (HasCallStack, Concurrent :> es, Foldable f)
  => Int
  -> (a -> Eff es b)
  -> f a
  -> Eff es ()
pooledMapConcurrentlyN_  threads f t = unsafeEff $ \es -> do
  U.pooledMapConcurrentlyN_ threads (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'UnliftIO.Async.pooledMapConcurrently_'.
pooledMapConcurrently_
  :: (HasCallStack, Concurrent :> es, Foldable f)
  => (a -> Eff es b)
  -> f a
  -> Eff es ()
pooledMapConcurrently_ f t = unsafeEff $ \es -> do
  U.pooledMapConcurrently_ (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'UnliftIO.Async.pooledForConcurrentlyN'.
pooledForConcurrentlyN
  :: (HasCallStack, Concurrent :> es, Traversable t)
  => Int
  -> t a
  -> (a -> Eff es b)
  -> Eff es (t b)
pooledForConcurrentlyN  threads t f = unsafeEff $ \es -> do
  U.pooledForConcurrentlyN threads t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledForConcurrently'.
pooledForConcurrently
  :: (HasCallStack, Concurrent :> es, Traversable t)
  => t a
  -> (a -> Eff es b)
  -> Eff es (t b)
pooledForConcurrently t f = unsafeEff $ \es -> do
  U.pooledForConcurrently t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledForConcurrentlyN_'.
pooledForConcurrentlyN_
  :: (HasCallStack, Concurrent :> es, Foldable f)
  => Int
  -> f a
  -> (a -> Eff es b)
  -> Eff es ()
pooledForConcurrentlyN_  threads t f = unsafeEff $ \es -> do
  U.pooledForConcurrentlyN_ threads t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledForConcurrently_'.
pooledForConcurrently_
  :: (HasCallStack, Concurrent :> es, Foldable f)
  => f a
  -> (a -> Eff es b)
  -> Eff es ()
pooledForConcurrently_ t f = unsafeEff $ \es -> do
  U.pooledForConcurrently_ t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledReplicateConcurrentlyN'.
pooledReplicateConcurrentlyN
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Int
  -> Eff es a
  -> Eff es [a]
pooledReplicateConcurrentlyN threads n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrentlyN threads n (unEff f =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledReplicateConcurrently'.
pooledReplicateConcurrently
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Eff es a
  -> Eff es [a]
pooledReplicateConcurrently n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrently n (unEff f =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledReplicateConcurrentlyN_'.
pooledReplicateConcurrentlyN_
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Int
  -> Eff es a
  -> Eff es ()
pooledReplicateConcurrentlyN_ threads n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrentlyN_ threads n (unEff f =<< cloneEnv es)

-- | Lifted 'UnliftIO.Async.pooledReplicateConcurrently_'.
pooledReplicateConcurrently_
  :: (HasCallStack, Concurrent :> es)
  => Int
  -> Eff es a
  -> Eff es ()
pooledReplicateConcurrently_ n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrently_ n (unEff f =<< cloneEnv es)

----------------------------------------
-- Conc

-- | Lifted t'UnliftIO.Async.Conc'.
data Conc :: [Effect] -> Type -> Type where
  Action :: Eff es a -> Conc es a
  Apply  :: Conc es (v -> a) -> Conc es v -> Conc es a
  LiftA2 :: (x -> y -> a) -> Conc es x -> Conc es y -> Conc es a
  Pure   :: a -> Conc es a
  Alt    :: Conc es a -> Conc es a -> Conc es a
  Empty  :: Conc es a

deriving stock instance Functor (Conc es)

instance Applicative (Conc es) where
  pure   = Pure
  (<*>)  = Apply
  (<*)   = LiftA2 (\x _ -> x)
  (*>)   = LiftA2 (\_ y -> y)
  liftA2 = LiftA2

instance Alternative (Conc es) where
  empty = Empty
  (<|>) = Alt

instance Semigroup a => Semigroup (Conc es a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Conc es a) where
  mempty = pure mempty

-- | Lifted 'UnliftIO.Async.conc'.
conc :: Eff es a -> Conc es a
conc = Action

-- | Lifted 'UnliftIO.Async.runConc'.
runConc :: (HasCallStack, Concurrent :> es) => Conc es a -> Eff es a
runConc m = unsafeEff $ \es -> U.runConc (unliftConc es m)
  where
    unliftConc :: Env es -> Conc es a -> U.Conc IO a
    unliftConc es = \case
      Action f     -> I.Action $ unEff f =<< cloneEnv es
      Apply a b    -> I.Apply (unliftConc es a) (unliftConc es b)
      LiftA2 f a b -> I.LiftA2 f (unliftConc es a) (unliftConc es b)
      Pure a       -> I.Pure a
      Alt a b      -> I.Alt (unliftConc es a) (unliftConc es b)
      Empty        -> I.Empty

----------------------------------------
-- Concurrently

-- | Lifted t'Control.Concurrent.Async.Concurrently'.
newtype Concurrently es a = Concurrently { runConcurrently :: Eff es a }

instance Functor (Concurrently es) where
  fmap f (Concurrently a) = Concurrently (fmap f a)

instance Concurrent :> es => Applicative (Concurrently es) where
  pure = Concurrently . pure
  Concurrently fs <*> Concurrently as =
    Concurrently ((\(f, a) -> f a) <$> concurrently fs as)

instance Concurrent :> es => Alternative (Concurrently es) where
  empty = Concurrently . unsafeEff_ . forever $ threadDelay maxBound
  Concurrently as <|> Concurrently bs =
    Concurrently (either id id <$> race as bs)

instance (Concurrent :> es, Semigroup a) => Semigroup (Concurrently es a) where
  (<>) = liftA2 (<>)

instance (Concurrent :> es, Monoid a) => Monoid (Concurrently es a) where
  mempty = pure mempty

----------------------------------------
-- Helpers

liftAsync
  :: HasCallStack
  => (IO a -> IO (Async a))
  -> Eff es a
  -> Eff es (Async a)
liftAsync fork action = unsafeEff $ \es -> do
  esA <- cloneEnv es
  fork $ unEff action esA

liftAsyncWithUnmask
  :: HasCallStack
  => (((forall b. IO b -> IO b) -> IO a) -> IO (Async a))
  -> ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
liftAsyncWithUnmask fork action = unsafeEff $ \es -> do
  esA <- cloneEnv es
  -- Unmask never runs its argument in a different thread.
  fork $ \unmask -> unEff (action $ reallyUnsafeLiftMapIO unmask) esA

liftWithAsync
  :: HasCallStack
  => (IO a -> (Async a -> IO b) -> IO b)
  -> Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
liftWithAsync withA action k = unsafeEff $ \es -> do
  esA <- cloneEnv es
  withA (unEff action esA)
        (\a -> unEff (k a) es)

liftWithAsyncWithUnmask
  :: HasCallStack
  => (((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b)
  -> ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
liftWithAsyncWithUnmask withA action k = unsafeEff $ \es -> do
  esA <- cloneEnv es
  -- Unmask never runs its argument in a different thread.
  withA (\unmask -> unEff (action $ reallyUnsafeLiftMapIO unmask) esA)
        (\a -> unEff (k a) es)
