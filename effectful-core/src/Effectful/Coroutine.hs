-- | 'Coroutine' gives you access to stream processing, similar to the
-- @conduit@, @pipes@ or @streaming@ libraries.  'Coroutine' is not a
-- "first class" coroutine: a coroutine computation can't be suspended
-- or passed around.

module Effectful.Coroutine
  ( -- * Effect
    Coroutine (..),

    -- ** Handlers

    forEach,
    yieldToList,
    yieldToReverseList,

    -- ** Operations

    call,
    yield,
    inFoldable,
  )
where

import Data.Foldable (traverse_)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.State.Static.Local (modify, runState)

data Coroutine a b :: Effect where
  Call :: a -> Coroutine a b m b

type instance DispatchOf (Coroutine a b) = Dynamic

-- | Call the 'Coroutine'.
call :: forall a b es. (Coroutine a b :> es) => a -> Eff es b
call a = send (Call a)

-- | The same as 'call', but specialised to the case when the call
-- returns @()@, for better type inference.
yield :: forall a es. (Coroutine a () :> es) => a -> Eff es ()
yield = call

-- | Handle the coroutine.
--
-- @
-- runPureEff $ execState 0 $ do
--     forEach \@Int (inFoldable \@Int [0 .. 5]) $ \\i -> do
--       modify (+ i)
-- 15
-- @
forEach :: forall a b es r. Eff (Coroutine a b ': es) r -> (a -> Eff es b) -> Eff es r
forEach caller call_ = handle caller
  where
    handle = interpret $ \_ -> \case
      Call a -> call_ a

-- | Yield all the elements of the @Foldable@.
inFoldable :: forall a t es. (Foldable t, Coroutine a () :> es) => t a -> Eff es ()
inFoldable = traverse_ yield -- can't use unannotated `call`; type inference fail

-- | Handle the coroutine by gathering the yielded values into a list.
--
-- @
-- runPureEff $ yieldToList $ do
--   for_ [0 .. 3] $ \\i -> do
--     yield \@Int i
--     yield (i * 10)
-- ([0,0,1,10,2,20,3,30],())
-- @
yieldToList :: forall a es r. Eff (Coroutine a () ': es) r -> Eff es ([a], r)
yieldToList =
  (fmap . fmap) (\(as, r) -> (reverse as, r)) yieldToReverseList

yieldToReverseList :: forall a es r. Eff (Coroutine a () ': es) r -> Eff es ([a], r)
yieldToReverseList = reinterpret @(Coroutine a ()) collect $ \_ -> \case
  Call x -> modify (x :)
  where
    collect = fmap (\(r, xs) -> (xs, r)) . runState []
