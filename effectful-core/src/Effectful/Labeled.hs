{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
module Effectful.Labeled
  ( -- * Effect
    Labeled
  , labeled

    -- ** Handlers
  , runLabeled
  ) where

import Data.Kind (Constraint)
import Unsafe.Coerce (unsafeCoerce)

import Effectful

-- | Demo:
--
-- >>> import Effectful.Reader.Static
--
-- >>> :{
--  runPureEff @String
--    . runLabeled @"a" (runReader "a")
--    . runLabeled @"b" (runReader "b")
--    $ do
--      a <- labeled @"a" @(Reader String) ask
--      b <- labeled @"b" @(Reader String) ask
--      pure $ a ++ b
-- :}
-- "ab"
--
data Labeled (label :: k) (e :: Effect) :: Effect

runLabeled
  :: forall label e es a b
   . (Eff (e : es) a -> Eff es b)
  -- ^ The effect handler.
  -> Eff (Labeled label e : es) a
  -> Eff es b
runLabeled runE m = unsafeCoerce runE m

labeled
  :: forall label e es a
   . Labeled label e :> es
  => (e :> es => Eff es a)
  -- ^ An action using the effect.
  -> Eff es a
labeled r = case fromLabeled @label @e @es Dict of Dict -> r

----------------------------------------
-- Helpers

data Dict (c :: Constraint) where
  Dict :: c => Dict c

fromLabeled :: Dict (Labeled label e :> es) -> Dict (e :> es)
fromLabeled = unsafeCoerce
