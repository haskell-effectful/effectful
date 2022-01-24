-- | The dynamically dispatched variant of the 'State' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
-- recommended to use one of the statically dispatched variants,
-- i.e. "Effectful.Writer.Static.Local" or "Effectful.Writer.Static.Shared".
module Effectful.Writer.Dynamic
  ( -- * Effect
    Writer(..)

    -- ** Handlers

    -- *** Local
  , runLocalWriter
  , execLocalWriter

    -- *** Shared
  , runSharedWriter
  , execSharedWriter

    -- * Operations
  , tell
  , listen
  , listens
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.Writer.Static.Local as L
import qualified Effectful.Writer.Static.Shared as S

-- | Provide access to a write only value of type @w@.
data Writer w :: Effect where
  Tell   :: w   -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

type instance DispatchOf (Writer w) = 'Dynamic

----------------------------------------
-- Local

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Local").
runLocalWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runLocalWriter = reinterpret L.runWriter localWriter

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Local").
execLocalWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execLocalWriter = reinterpret L.execWriter localWriter

localWriter
  :: (L.Writer w :> es, Monoid w)
  => LocalEnv localEs es
  -> Writer w (Eff localEs) a
  -> Eff es a
localWriter env = \case
  Tell w   -> L.tell w
  Listen m -> localSeqUnlift env $ \unlift -> L.listen (unlift m)

----------------------------------------
-- Shared

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Shared").
runSharedWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runSharedWriter = reinterpret S.runWriter sharedWriter

-- | Run the 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Shared").
execSharedWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execSharedWriter = reinterpret S.execWriter sharedWriter

sharedWriter
  :: (S.Writer w :> es, Monoid w)
  => LocalEnv localEs es
  -> Writer w (Eff localEs) a
  -> Eff es a
sharedWriter env = \case
  Tell w    -> S.tell w
  Listen m  -> localSeqUnlift env $ \unlift -> S.listen (unlift m)

----------------------------------------
-- Operations

-- | Append the given output to the overall output of the 'Writer'.
tell
  :: (HasCallStack, Writer w :> es)
  => w
  -> Eff es ()
tell = send . Tell

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
listen
  :: (HasCallStack, Writer w :> es)
  => Eff es a
  -> Eff es (a, w)
listen = send . Listen

-- | Execute an action and append its output to the overall output of the
-- 'Writer', then return the final value along with a function of the recorded
-- output.
--
-- @'listens' f m ≡ 'Data.Bifunctor.second' f '<$>' 'listen' m@
listens
  :: (HasCallStack, Writer w :> es)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
