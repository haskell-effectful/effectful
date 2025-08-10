module Effectful.ST (STE, runSTE, liftST) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import Effectful
import Effectful.Dispatch.Static

-- | An effect for embedding 'ST' computations
data STE (s :: Type) :: Effect
type role STE nominal phantom phantom

type instance DispatchOf (STE s) = Static NoSideEffects
data instance StaticRep (STE s) = STE

-- | Run the 'STE' effect.
-- Since effectful allows several 'STE' effects to be in scope at once,
-- it can be ambiguous which one a usage of 'liftST' refers to.
-- In these cases, the 'Proxy' parameter can be used to disambiguate.
-- In particular, instead of @liftST $ newSTRef x@, you will have to write 
-- @liftST \@s $ newSTRef x@.
runSTE :: (forall s. Proxy s -> Eff (STE s : es) a) -> Eff es a
runSTE eff = evalStaticRep STE (eff Proxy)

liftST :: forall s a es. (STE s :> es) => ST s a -> Eff es a
liftST st = unsafeEff_ (unsafeSTToIO st)
