{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
-- | Labeled effects.
module Effectful.Labeled
  ( -- * Example
    -- $example

    -- * Effect
    Labeled

    -- ** Handlers
  , runLabeled

    -- ** Operations
  , labeled
  ) where

import Unsafe.Coerce (unsafeCoerce)

import Effectful
import Effectful.Dispatch.Static

-- $example
--
-- An effect can be assigned multiple labels and you can have all of them
-- available at the same time.
--
-- >>> import Effectful.Reader.Static
--
-- >>> :{
--  action
--    :: ( Labeled "a" (Reader String) :> es
--       , Labeled "b" (Reader String) :> es
--       , Reader String :> es
--       )
--    => Eff es String
--  action = do
--    a <- labeled @"b" @(Reader String) $ do
--      labeled @"a" @(Reader String) $ do
--        ask
--    b <- labeled @"b" @(Reader String) $ do
--      ask
--    pure $ a ++ b
-- :}
--
-- >>> :{
--  runPureEff @String
--    . runLabeled @"a" (runReader "a")
--    . runLabeled @"b" (runReader "b")
--    . runReader "c"
--    $ action
-- :}
-- "ab"

-- | Assign a label to an effect.
data Labeled (label :: k) (e :: Effect) :: Effect

type instance DispatchOf (Labeled label e) = Static NoSideEffects

data instance StaticRep (Labeled label e)

-- | Run a 'Labeled' effect with a given effect handler.
runLabeled
  :: forall label e es a b
   . (Eff (e : es) a -> Eff es b)
  -- ^ The effect handler.
  -> Eff (Labeled label e : es) a
  -> Eff es b
runLabeled runE m = runE (fromLabeled m)

-- | Bring an effect into scope to be able to run its operations.
labeled
  :: forall label e es a
   . Labeled label e :> es
  => Eff (e : es) a
  -- ^ The action using the effect.
  -> Eff es a
labeled m = subsume @(Labeled label e) (toLabeled m)

----------------------------------------
-- Helpers

fromLabeled :: Eff (Labeled label e : es) a -> Eff (e : es) a
fromLabeled = unsafeCoerce

toLabeled :: Eff (e : es) a -> Eff (Labeled label e : es) a
toLabeled = unsafeCoerce
