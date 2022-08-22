module Effectful.Concurrent.Effect
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent
  ) where

import Effectful
import Effectful.Dispatch.Static

-- | Provide the ability to run 'Eff' computations concurrently in multiple
-- threads and communicate between them.
--
-- /Warning:/ unless you stick to high level functions from the
-- 'Effectful.Concurrent.Async.withAsync' family, the 'Concurrent' effect makes
-- it possible to escape the scope of any scoped effect operation. Consider the
-- following:
--
-- >>> import qualified Effectful.Reader.Static as R
--
-- >>> printAsk msg = liftIO . putStrLn . (msg ++) . (": " ++) =<< R.ask
--
-- >>> :{
--   runEff . R.runReader "GLOBAL" . runConcurrent $ do
--     a <- R.local (const "LOCAL") $ do
--       a <- async $ do
--         printAsk "child (first)"
--         threadDelay 20000
--         printAsk "child (second)"
--       threadDelay 10000
--       printAsk "parent (inside)"
--       pure a
--     printAsk "parent (outside)"
--     wait a
-- :}
-- child (first): LOCAL
-- parent (inside): LOCAL
-- parent (outside): GLOBAL
-- child (second): LOCAL
--
-- Note that the asynchronous computation doesn't respect the scope of
-- 'Effectful.Reader.Static.local', i.e. the child thread still behaves like
-- it's inside the 'Effectful.Reader.Static.local' block, even though the parent
-- thread already got out of it.
--
-- This is because the value provided by the t'Effectful.Reader.Static.Reader'
-- effect is thread local, i.e. each thread manages its own version of it. For
-- the t'Effectful.Reader.Static.Reader' it is the only reasonable behavior, it
-- wouldn't be very useful if its "read only" value was affected by calls to
-- 'Effectful.Reader.Static.local' from its parent or child threads.
--
-- However, the cut isn't so clear if it comes to effects that provide access to
-- a mutable state. That's why statically dispatched @State@ and @Writer@
-- effects come in two flavors, local and shared:
--
-- >>> import qualified Effectful.State.Static.Local as SL
-- >>> :{
--   runEff . SL.execState "Hi" . runConcurrent $ do
--     replicateConcurrently_ 3 $ SL.modify (++ "!")
-- :}
-- "Hi"
--
-- >>> import qualified Effectful.State.Static.Shared as SS
-- >>> :{
--   runEff . SS.execState "Hi" . runConcurrent $ do
--     replicateConcurrently_ 3 $ SS.modify (++ "!")
-- :}
-- "Hi!!!"
--
-- In the first example state updates made concurrently are not reflected in the
-- parent thread because the value is thread local, but in the second example
-- they are, because the value is shared.
--
data Concurrent :: Effect

type instance DispatchOf Concurrent = Static WithSideEffects
data instance StaticRep Concurrent = Concurrent

-- | Run the 'Concurrent' effect.
runConcurrent :: IOE :> es => Eff (Concurrent : es) a -> Eff es a
runConcurrent = evalStaticRep Concurrent

-- $setup
-- >>> import Effectful.Concurrent
-- >>> import Effectful.Concurrent.Async
