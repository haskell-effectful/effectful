{-# LANGUAGE UndecidableInstances #-}
module Effectful.Async
  ( -- * Async effect
    AsyncE
  , runAsyncE

  -- * Asynchronous actions
  , Async

  -- * High-level API

  -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask
  , withAsyncOnWithUnmask

  -- ** Querying 'Async's
  , wait, poll, waitCatch, A.asyncThreadId
  , cancel, uninterruptibleCancel, cancelWith, A.AsyncCancelled(..)
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
import qualified Control.Concurrent.Async as A
import qualified UnliftIO.Async as U
import qualified UnliftIO.Internals.Async as I

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Run 'Eff' operations asynchronously via the @async@ library.
--
-- /Note:/ thread local state changes in 'Eff' operations run asynchronously
-- will not affect the parent thread.
--
-- /TODO:/ write about 'AsyncE' not respecting scoped operations.
data AsyncE :: Effect where
  AsyncE :: AsyncE m r

runAsyncE :: IOE :> es => Eff (AsyncE : es) a -> Eff es a
runAsyncE = evalEffect (IdE AsyncE)

----------------------------------------
-- Operations

-- | Lifted 'A.async'.
async :: AsyncE :> es => Eff es a -> Eff es (Async a)
async = liftAsync A.async

-- | Lifted 'A.asyncBound'.
asyncBound :: AsyncE :> es => Eff es a -> Eff es (Async a)
asyncBound = liftAsync A.asyncBound

-- | Lifted 'A.asyncOn'.
asyncOn :: AsyncE :> es => Int -> Eff es a -> Eff es (Async a)
asyncOn cpu = liftAsync (A.asyncOn cpu)

-- | Lifted 'A.asyncWithUnmask'.
asyncWithUnmask
  :: AsyncE :> es
  => ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
asyncWithUnmask = liftAsyncWithUnmask A.asyncWithUnmask

-- | Lifted 'A.asyncOnWithUnmask'.
asyncOnWithUnmask
  :: AsyncE :> es
  => Int
  -> ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
asyncOnWithUnmask cpu = liftAsyncWithUnmask (A.asyncOnWithUnmask cpu)

-- | Lifted 'A.withAsync'.
withAsync
  :: AsyncE :> es
  => Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsync = liftWithAsync A.withAsync

-- | Lifted 'A.withAsyncBound'.
withAsyncBound
  :: AsyncE :> es
  => Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncBound = liftWithAsync A.withAsyncBound

-- | Lifted 'A.withAsyncOn'.
withAsyncOn
  :: AsyncE :> es
  => Int
  -> Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncOn cpu = liftWithAsync (A.withAsyncOn cpu)

-- | Lifted 'A.withAsyncWithUnmask'.
withAsyncWithUnmask
  :: AsyncE :> es
  => ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncWithUnmask = liftWithAsyncWithUnmask A.withAsyncWithUnmask

-- | Lifted 'A.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask
  :: AsyncE :> es
  => Int
  -> ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncOnWithUnmask cpu = liftWithAsyncWithUnmask (A.withAsyncOnWithUnmask cpu)

-- | Lifted 'A.wait'.
wait :: AsyncE :> es => Async a -> Eff es a
wait = unsafeEff_ . A.wait

-- | Lifted 'A.poll'.
poll
  :: AsyncE :> es
  => Async a
  -> Eff es (Maybe (Either SomeException a))
poll = unsafeEff_ . A.poll

-- | Lifted 'A.cancel'.
cancel :: AsyncE :> es => Async a -> Eff es ()
cancel = unsafeEff_ . A.cancel

-- | Lifted 'A.cancelWith'.
cancelWith :: (Exception e, AsyncE :> es) => Async a -> e -> Eff es ()
cancelWith a = unsafeEff_ . A.cancelWith a

-- | Lifted 'A.uninterruptibleCancel'.
uninterruptibleCancel :: AsyncE :> es => Async a -> Eff es ()
uninterruptibleCancel = unsafeEff_ . A.uninterruptibleCancel

-- | Lifted 'A.waitCatch'.
waitCatch
  :: AsyncE :> es
  => Async a
  -> Eff es (Either SomeException a)
waitCatch = unsafeEff_ . A.waitCatch

-- | Lifted 'A.waitAny'.
waitAny :: AsyncE :> es => [Async a] -> Eff es (Async a, a)
waitAny = unsafeEff_ . A.waitAny

-- | Lifted 'A.waitAnyCatch'.
waitAnyCatch
  :: AsyncE :> es
  => [Async a]
  -> Eff es (Async a, Either SomeException a)
waitAnyCatch = unsafeEff_ . A.waitAnyCatch

-- | Lifted 'A.waitAnyCancel'.
waitAnyCancel :: AsyncE :> es => [Async a] -> Eff es (Async a, a)
waitAnyCancel = unsafeEff_ . A.waitAnyCancel

-- | Lifted 'A.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: AsyncE :> es
  => [Async a]
  -> Eff es (Async a, Either SomeException a)
waitAnyCatchCancel = unsafeEff_ . A.waitAnyCatchCancel

-- | Lifted 'A.waitEither'.
waitEither
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either a b)
waitEither a b = unsafeEff_ $ A.waitEither a b

-- | Lifted 'A.waitEitherCatch'.
waitEitherCatch
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b = unsafeEff_ $ A.waitEitherCatch a b

-- | Lifted 'A.waitEitherCancel'.
waitEitherCancel
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either a b)
waitEitherCancel a b = unsafeEff_ $ A.waitEitherCancel a b

-- | Lifted 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b = unsafeEff_  $ A.waitEitherCatch a b

-- | Lifted 'A.waitEither_'.
waitEither_ :: AsyncE :> es => Async a -> Async b -> Eff es ()
waitEither_ a b = unsafeEff_ $ A.waitEither_ a b

-- | Lifted 'A.waitBoth'.
waitBoth :: AsyncE :> es => Async a -> Async b -> Eff es (a, b)
waitBoth a b = unsafeEff_ $ A.waitBoth a b

-- | Lifted 'A.link'.
link :: AsyncE :> es => Async a -> Eff es ()
link = unsafeEff_ . A.link

-- | Lifted 'A.linkOnly'.
linkOnly :: AsyncE :> es => (SomeException -> Bool) -> Async a -> Eff es ()
linkOnly f = unsafeEff_ . A.linkOnly f

-- | Lifted 'A.link2'.
link2 :: AsyncE :> es => Async a -> Async b -> Eff es ()
link2 a b = unsafeEff_ $ A.link2 a b

-- | Lifted 'A.link2Only'.
link2Only :: AsyncE :> es => (SomeException -> Bool) -> Async a -> Async b -> Eff es ()
link2Only f a b = unsafeEff_ $ A.link2Only f a b

-- | Lifted 'A.race'.
race :: AsyncE :> es => Eff es a -> Eff es b -> Eff es (Either a b)
race ma mb = unsafeEff $ \es -> do
  A.race (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- | Lifted 'A.race_'.
race_ ::  AsyncE :> es => Eff es a -> Eff es b -> Eff es ()
race_ ma mb = unsafeEff $ \es -> do
  A.race_ (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- | Lifted 'A.concurrently'.
concurrently :: AsyncE :> es => Eff es a -> Eff es b -> Eff es (a, b)
concurrently ma mb = unsafeEff $ \es -> do
  A.concurrently (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- | Lifted 'A.concurrently_'.
concurrently_ :: AsyncE :> es => Eff es a -> Eff es b -> Eff es ()
concurrently_ ma mb = unsafeEff $ \es -> do
  A.concurrently_ (unEff ma =<< cloneEnv es) (unEff mb =<< cloneEnv es)

-- Below functions use variants from the @unliftio@ library as they minimize the
-- amount of spawned threads and are thus much more efficient than the ones from
-- the @async@ library.

-- | Lifted 'A.mapConcurrently'.
mapConcurrently
  :: (Traversable f, AsyncE :> es)
  => (a -> Eff es b)
  -> f a
  -> Eff es (f b)
mapConcurrently f t = unsafeEff $ \es -> do
  U.mapConcurrently (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'A.mapConcurrently_'.
mapConcurrently_
  :: (Foldable f, AsyncE :> es)
  => (a -> Eff es b)
  -> f a
  -> Eff es ()
mapConcurrently_ f t = unsafeEff $ \es -> do
  U.mapConcurrently_ (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'A.forConcurrently'.
forConcurrently
  :: (Traversable f, AsyncE :> es)
  => f a
  -> (a -> Eff es b)
  -> Eff es (f b)
forConcurrently t f = unsafeEff $ \es -> do
  U.forConcurrently t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'A.forConcurrently_'.
forConcurrently_
  :: (Foldable f, AsyncE :> es)
  => f a
  -> (a -> Eff es b)
  -> Eff es ()
forConcurrently_ t f = unsafeEff $ \es -> do
  U.forConcurrently_ t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'A.replicateConcurrently'.
replicateConcurrently :: AsyncE :> es => Int -> Eff es a -> Eff es [a]
replicateConcurrently n f = unsafeEff $ \es -> do
  U.replicateConcurrently n (unEff f =<< cloneEnv es)

-- | Lifted 'A.replicateConcurrently_'.
replicateConcurrently_ :: AsyncE :> es => Int -> Eff es a -> Eff es ()
replicateConcurrently_ n f = unsafeEff $ \es -> do
  U.replicateConcurrently_ n (unEff f =<< cloneEnv es)

----------------------------------------
-- Pooled concurrency (unliftio)

-- | Lifted 'U.pooledMapConcurrentlyN'.
pooledMapConcurrentlyN
  :: (AsyncE :> es, Traversable t)
  => Int
  -> (a -> Eff es b)
  -> t a
  -> Eff es (t b)
pooledMapConcurrentlyN  threads f t = unsafeEff $ \es -> do
  U.pooledMapConcurrentlyN threads (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'U.pooledMapConcurrently'.
pooledMapConcurrently
  :: (AsyncE :> es, Traversable t)
  => (a -> Eff es b)
  -> t a
  -> Eff es (t b)
pooledMapConcurrently f t = unsafeEff $ \es -> do
  U.pooledMapConcurrently (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'U.pooledMapConcurrentlyN'.
pooledMapConcurrentlyN_
  :: (AsyncE :> es, Foldable f)
  => Int
  -> (a -> Eff es b)
  -> f a
  -> Eff es ()
pooledMapConcurrentlyN_  threads f t = unsafeEff $ \es -> do
  U.pooledMapConcurrentlyN_ threads (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'U.pooledMapConcurrently_'.
pooledMapConcurrently_
  :: (AsyncE :> es, Foldable f)
  => (a -> Eff es b)
  -> f a
  -> Eff es ()
pooledMapConcurrently_ f t = unsafeEff $ \es -> do
  U.pooledMapConcurrently_ (\a -> unEff (f a) =<< cloneEnv es) t

-- | Lifted 'U.pooledForConcurrentlyN'.
pooledForConcurrentlyN
  :: (AsyncE :> es, Traversable t)
  => Int
  -> t a
  -> (a -> Eff es b)
  -> Eff es (t b)
pooledForConcurrentlyN  threads t f = unsafeEff $ \es -> do
  U.pooledForConcurrentlyN threads t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'U.pooledForConcurrently'.
pooledForConcurrently
  :: (AsyncE :> es, Traversable t)
  => t a
  -> (a -> Eff es b)
  -> Eff es (t b)
pooledForConcurrently t f = unsafeEff $ \es -> do
  U.pooledForConcurrently t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'U.pooledForConcurrentlyN'.
pooledForConcurrentlyN_
  :: (AsyncE :> es, Foldable f)
  => Int
  -> f a
  -> (a -> Eff es b)
  -> Eff es ()
pooledForConcurrentlyN_  threads t f = unsafeEff $ \es -> do
  U.pooledForConcurrentlyN_ threads t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'U.pooledForConcurrently_'.
pooledForConcurrently_
  :: (AsyncE :> es, Foldable f)
  => f a
  -> (a -> Eff es b)
  -> Eff es ()
pooledForConcurrently_ t f = unsafeEff $ \es -> do
  U.pooledForConcurrently_ t (\a -> unEff (f a) =<< cloneEnv es)

-- | Lifted 'U.pooledReplicateConcurrentlyN'.
pooledReplicateConcurrentlyN :: AsyncE :> es => Int -> Int -> Eff es a -> Eff es [a]
pooledReplicateConcurrentlyN threads n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrentlyN threads n (unEff f =<< cloneEnv es)

-- | Lifted 'U.pooledReplicateConcurrently'.
pooledReplicateConcurrently :: AsyncE :> es => Int -> Eff es a -> Eff es [a]
pooledReplicateConcurrently n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrently n (unEff f =<< cloneEnv es)

-- | Lifted 'U.pooledReplicateConcurrentlyN_'.
pooledReplicateConcurrentlyN_ :: AsyncE :> es => Int -> Int -> Eff es a -> Eff es ()
pooledReplicateConcurrentlyN_ threads n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrentlyN_ threads n (unEff f =<< cloneEnv es)

-- | Lifted 'U.pooledReplicateConcurrently_'.
pooledReplicateConcurrently_ :: AsyncE :> es => Int -> Eff es a -> Eff es ()
pooledReplicateConcurrently_ n f = unsafeEff $ \es -> do
  U.pooledReplicateConcurrently_ n (unEff f =<< cloneEnv es)

----------------------------------------
-- Conc

-- | Lifted 'U.Conc'.
data Conc es a where
  Action :: Eff es a -> Conc es a
  Apply  :: Conc es (v -> a) -> Conc es v -> Conc es a
  LiftA2 :: (x -> y -> a) -> Conc es x -> Conc es y -> Conc es a
  Pure   :: a -> Conc es a
  Alt    :: Conc es a -> Conc es a -> Conc es a
  Empty  :: Conc es a

deriving instance Functor (Conc es)

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

-- | Lifted 'U.conc'.
conc :: Eff es a -> Conc es a
conc = Action

-- | Lifted 'U.runConc'.
runConc :: AsyncE :> es => Conc es a -> Eff es a
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

-- | Lifted 'A.Concurrently'.
newtype Concurrently es a = Concurrently { runConcurrently :: Eff es a }

instance Functor (Concurrently es) where
  fmap f (Concurrently a) = Concurrently (fmap f a)

instance AsyncE :> es => Applicative (Concurrently es) where
  pure = Concurrently . pure
  Concurrently fs <*> Concurrently as =
    Concurrently ((\(f, a) -> f a) <$> concurrently fs as)

instance AsyncE :> es => Alternative (Concurrently es) where
  empty = Concurrently . unsafeEff_ . forever $ threadDelay maxBound
  Concurrently as <|> Concurrently bs =
    Concurrently (either id id <$> race as bs)

instance (AsyncE :> es, Semigroup a) => Semigroup (Concurrently es a) where
  (<>) = liftA2 (<>)

instance (AsyncE :> es, Monoid a) => Monoid (Concurrently es a) where
  mempty = pure mempty

----------------------------------------
-- Helpers

liftAsync
  :: (IO a -> IO (Async a))
  -> Eff es a
  -> Eff es (Async a)
liftAsync fork action = unsafeEff $ \es -> do
  esA <- cloneEnv es
  fork $ unEff action esA

liftAsyncWithUnmask
  :: (((forall b. IO b -> IO b) -> IO a) -> IO (Async a))
  -> ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
liftAsyncWithUnmask fork action = do
  unsafeWithLiftMapIO $ \liftMap -> unsafeEff $ \es -> do
    esA <- cloneEnv es
    fork $ \unmask -> unEff (action $ liftMap unmask) esA

liftWithAsync
  :: (IO a -> (Async a -> IO b) -> IO b)
  -> Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
liftWithAsync withA action k = unsafeEff $ \es -> do
  esA <- cloneEnv es
  withA (unEff action esA)
        (\a -> unEff (k a) es)

liftWithAsyncWithUnmask
  :: (((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b)
  -> ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
liftWithAsyncWithUnmask withA action k = do
  unsafeWithLiftMapIO $ \liftMap -> unsafeEff $ \es -> do
    esA <- cloneEnv es
    withA (\unmask -> unEff (action $ liftMap unmask) esA)
          (\a -> unEff (k a) es)
