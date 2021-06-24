{-# LANGUAGE UndecidableInstances #-}
module Effectful.Async
  ( -- * Async effect
    AsyncE
  , runAsyncE

    -- * Asynchronous actions
  , A.Async
    -- ** Spawning
  , async, asyncBound, asyncOn
  , asyncWithUnmask, asyncOnWithUnmask

    -- ** Spawning with automatic 'cancel'ation
  , withAsync, withAsyncBound, withAsyncOn
  , withAsyncWithUnmask, withAsyncOnWithUnmask

    -- ** Quering 'Async's
  , wait, poll, waitCatch
  , cancel
  , uninterruptibleCancel
  , cancelWith
  , A.asyncThreadId
  , A.AsyncCancelled(..)

    -- ** STM operations
  , A.waitSTM, A.pollSTM, A.waitCatchSTM

    -- ** Waiting for multiple 'Async's
  , waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel
  , waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel
  , waitEither_
  , waitBoth

    -- ** Waiting for multiple 'Async's in STM
  , A.waitAnySTM
  , A.waitAnyCatchSTM
  , A.waitEitherSTM
  , A.waitEitherCatchSTM
  , A.waitEitherSTM_
  , A.waitBothSTM

    -- ** Linking
  , link, link2
  , A.ExceptionInLinkedThread(..)

    -- * Convenient utilities
  , race, race_, concurrently, concurrently_
  , mapConcurrently, mapConcurrently_
  , forConcurrently, forConcurrently_
  , replicateConcurrently, replicateConcurrently_
  , Concurrently(..)

  , A.compareAsyncs
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Exception (Exception, SomeException)
import Control.Monad (forever, void)
import Data.Foldable (fold)
import qualified Control.Concurrent.Async as A

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Run 'Eff' computations asynchronously using the @async@ library.
--
-- /Note:/ thread local state changes in 'Eff' computations lifted into an
-- 'Async' will not affect the parent thread.
data AsyncE :: Effect where
  AsyncE :: AsyncE m r

runAsyncE :: IOE :> es => Eff (AsyncE : es) a -> Eff es a
runAsyncE = evalEffect (IdE AsyncE)

-- | Generalized version of 'A.async'.
async :: AsyncE :> es => Eff es a -> Eff es (Async a)
async = liftAsync A.async

-- | Generalized version of 'A.asyncBound'.
asyncBound :: AsyncE :> es => Eff es a -> Eff es (Async a)
asyncBound = liftAsync A.asyncBound

-- | Generalized version of 'A.asyncOn'.
asyncOn :: AsyncE :> es => Int -> Eff es a -> Eff es (Async a)
asyncOn cpu = liftAsync (A.asyncOn cpu)

-- | Generalized version of 'A.asyncWithUnmask'.
asyncWithUnmask
  :: AsyncE :> es
  => ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
asyncWithUnmask = liftAsyncWithUnmask A.asyncWithUnmask

-- | Generalized version of 'A.asyncOnWithUnmask'.
asyncOnWithUnmask
  :: AsyncE :> es
  => Int
  -> ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
asyncOnWithUnmask cpu = liftAsyncWithUnmask (A.asyncOnWithUnmask cpu)

-- | Generalized version of 'A.withAsync'.
withAsync
  :: AsyncE :> es
  => Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsync = liftWithAsync A.withAsync

-- | Generalized version of 'A.withAsyncBound'.
withAsyncBound
  :: AsyncE :> es
  => Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncBound = liftWithAsync A.withAsyncBound

-- | Generalized version of 'A.withAsyncOn'.
withAsyncOn
  :: AsyncE :> es
  => Int
  -> Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncOn cpu = liftWithAsync (A.withAsyncOn cpu)

-- | Generalized version of 'A.withAsyncWithUnmask'.
withAsyncWithUnmask
  :: AsyncE :> es
  => ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncWithUnmask = liftWithAsyncWithUnmask A.withAsyncWithUnmask

-- | Generalized version of 'A.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask
  :: AsyncE :> es
  => Int
  -> ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
withAsyncOnWithUnmask cpu = liftWithAsyncWithUnmask (A.withAsyncOnWithUnmask cpu)

-- | Generalized version of 'A.wait'.
wait :: AsyncE :> es => Async a -> Eff es a
wait = unsafeEff_ . A.wait

-- | Generalized version of 'A.poll'.
poll
  :: AsyncE :> es
  => Async a
  -> Eff es (Maybe (Either SomeException a))
poll = unsafeEff_ . A.poll

-- | Generalized version of 'A.cancel'.
cancel :: AsyncE :> es => Async a -> Eff es ()
cancel = unsafeEff_ . A.cancel

-- | Generalized version of 'A.cancelWith'.
cancelWith :: (Exception e, AsyncE :> es) => Async a -> e -> Eff es ()
cancelWith a = unsafeEff_ . A.cancelWith a

-- | Generalized version of 'A.uninterruptibleCancel'.
uninterruptibleCancel :: AsyncE :> es => Async a -> Eff es ()
uninterruptibleCancel = unsafeEff_ . A.uninterruptibleCancel

-- | Generalized version of 'A.waitCatch'.
waitCatch
  :: AsyncE :> es
  => Async a
  -> Eff es (Either SomeException a)
waitCatch = unsafeEff_ . A.waitCatch

-- | Generalized version of 'A.waitAny'.
waitAny :: AsyncE :> es => [Async a] -> Eff es (Async a, a)
waitAny = unsafeEff_ . A.waitAny

-- | Generalized version of 'A.waitAnyCatch'.
waitAnyCatch
  :: AsyncE :> es
  => [Async a]
  -> Eff es (Async a, Either SomeException a)
waitAnyCatch = unsafeEff_ . A.waitAnyCatch

-- | Generalized version of 'A.waitAnyCancel'.
waitAnyCancel :: AsyncE :> es => [Async a] -> Eff es (Async a, a)
waitAnyCancel = unsafeEff_ . A.waitAnyCancel

-- | Generalized version of 'A.waitAnyCatchCancel'.
waitAnyCatchCancel
  :: AsyncE :> es
  => [Async a]
  -> Eff es (Async a, Either SomeException a)
waitAnyCatchCancel = unsafeEff_ . A.waitAnyCatchCancel

-- | Generalized version of 'A.waitEither'.
waitEither
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either a b)
waitEither a b = unsafeEff_ $ A.waitEither a b

-- | Generalized version of 'A.waitEitherCatch'.
waitEitherCatch
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch a b = unsafeEff_ $ A.waitEitherCatch a b

-- | Generalized version of 'A.waitEitherCancel'.
waitEitherCancel
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either a b)
waitEitherCancel a b = unsafeEff_ $ A.waitEitherCancel a b

-- | Generalized version of 'A.waitEitherCatchCancel'.
waitEitherCatchCancel
  :: AsyncE :> es
  => Async a
  -> Async b
  -> Eff es (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel a b = unsafeEff_  $ A.waitEitherCatch a b

-- | Generalized version of 'A.waitEither_'.
waitEither_ :: AsyncE :> es => Async a -> Async b -> Eff es ()
waitEither_ a b = unsafeEff_ $ A.waitEither_ a b

-- | Generalized version of 'A.waitBoth'.
waitBoth :: AsyncE :> es => Async a -> Async b -> Eff es (a, b)
waitBoth a b = unsafeEff_ $ A.waitBoth a b

-- | Generalized version of 'A.link'.
link :: AsyncE :> es => Async a -> Eff es ()
link = unsafeEff_ . A.link

-- | Generalized version of 'A.link2'.
link2 :: AsyncE :> es => Async a -> Async b -> Eff es ()
link2 a b = unsafeEff_ $ A.link2 a b

-- | Generalized version of 'A.race'.
race :: AsyncE :> es => Eff es a -> Eff es b -> Eff es (Either a b)
race ma mb = withAsync ma $ \a -> withAsync mb $ \b -> waitEither a b

-- | Generalized version of 'A.race_'.
race_ ::  AsyncE :> es => Eff es a -> Eff es b -> Eff es ()
race_ ma mb = withAsync ma $ \a -> withAsync mb $ \b -> waitEither_ a b

-- | Generalized version of 'A.concurrently'.
concurrently :: AsyncE :> es => Eff es a -> Eff es b -> Eff es (a, b)
concurrently ma mb = withAsync ma $ \a -> withAsync mb $ \b -> waitBoth a b

-- | Generalized version of 'A.concurrently_'.
concurrently_ :: AsyncE :> es => Eff es a -> Eff es b -> Eff es ()
concurrently_ ma mb = void $ concurrently ma mb

-- | Generalized version of 'A.mapConcurrently'.
mapConcurrently
  :: (Traversable f, AsyncE :> es)
  => (a -> Eff es b)
  -> f a
  -> Eff es (f b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- | Generalized version of 'A.mapConcurrently_'.
mapConcurrently_
  :: (Foldable f, AsyncE :> es)
  => (a -> Eff es b)
  -> f a
  -> Eff es ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)

-- | Generalized version of 'A.forConcurrently'.
forConcurrently
  :: (Traversable f, AsyncE :> es)
  => f a
  -> (a -> Eff es b)
  -> Eff es (f b)
forConcurrently = flip mapConcurrently

-- | Generalized version of 'A.forConcurrently_'.
forConcurrently_
  :: (Foldable f, AsyncE :> es)
  => f a
  -> (a -> Eff es b)
  -> Eff es ()
forConcurrently_ = flip mapConcurrently_

-- | Generalized version of 'A.replicateConcurrently'.
replicateConcurrently :: AsyncE :> es => Int -> Eff es a -> Eff es [a]
replicateConcurrently n =
  runConcurrently . sequenceA . replicate n . Concurrently

-- | Generalized version of 'A.replicateConcurrently_'.
replicateConcurrently_ :: AsyncE :> es => Int -> Eff es a -> Eff es ()
replicateConcurrently_ n =
  runConcurrently . fold . replicate n . Concurrently . void

----------------------------------------

-- | Generalized version of 'A.Concurrently'.
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
liftAsync fork action = unsafeEff $ \es0 -> do
  es <- cloneEnv es0
  fork $ unEff action es

liftAsyncWithUnmask
  :: (((forall b. IO b -> IO b) -> IO a) -> IO (Async a))
  -> ((forall b. Eff es b -> Eff es b) -> Eff es a)
  -> Eff es (Async a)
liftAsyncWithUnmask fork action = unsafeEff $ \es0 -> do
  es <- cloneEnv es0
  fork $ \unmask -> unEff (action $ \m -> unsafeEff $ unmask . unEff m) es

liftWithAsync
  :: (IO a -> (Async a -> IO b) -> IO b)
  -> Eff es a
  -> (Async a -> Eff es b)
  -> Eff es b
liftWithAsync withA action k = unsafeEff $ \es0 -> do
  es <- cloneEnv es0
  withA (unEff action es)
        (\a -> unEff (k a) es0)

liftWithAsyncWithUnmask
  :: (((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b)
  -> ((forall c. Eff es c -> Eff es c) -> Eff es a)
  -> (Async a -> Eff es b)
  -> Eff es b
liftWithAsyncWithUnmask withA action k = unsafeEff $ \es0 -> do
  es <- cloneEnv es0
  withA (\unmask -> unEff (action $ \m -> unsafeEff $ unmask . unEff m) es)
        (\a -> unEff (k a) es0)
