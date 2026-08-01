-- | Support for early return from a computation.
--
-- >>> import Control.Monad (when)
--
-- >>> :{
--   classify :: ReturnWith String :> es => Int -> Eff es String
--   classify n = do
--     when (n < 0) $ returnWith "negative"
--     when (n == 0) $ returnWith "zero"
--     pure "positive"
-- :}
--
-- >>> runEff . runReturnWith $ classify 5
-- "positive"
--
-- >>> runEff . runReturnWith $ classify (-5)
-- "negative"
--
-- @since 2.7.0.0
module Effectful.ReturnWith.Static
  ( -- * Effect
    ReturnWith

    -- ** Handlers
  , runReturnWith

    -- ** Operations
  , returnWith
  ) where

import Data.Kind
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception
import Effectful.Internal.Utils

-- | Provide the ability to return early with a value of type @r@.
data ReturnWith (r :: Type) :: Effect

type instance DispatchOf (ReturnWith r) = Static NoSideEffects
newtype instance StaticRep (ReturnWith r) = ReturnWith ReturnWithId

-- | Run a computation that can return early with a value of type @r@.
runReturnWith
  :: forall r es
   . HasCallStack
  => Eff (ReturnWith r : es) r
  -> Eff es r
runReturnWith action = do
  rid <- unsafeEff_ newReturnWithId
  evalStaticRep (ReturnWith @r rid) $ do
    catchJust (matchReturnWith rid) action pure

-- | Return early with the given value.
returnWith
  :: forall r es a. (HasCallStack, ReturnWith r :> es)
  => r
  -- ^ The value.
  -> Eff es a
returnWith r = do
  ReturnWith rid <- getStaticRep @(ReturnWith r)
  withFrozenCallStack throwIO $ ReturnWithWrapper rid callStack (toAny r)

----------------------------------------
-- Helpers

newtype ReturnWithId = ReturnWithId Unique
  deriving newtype Eq

-- | A unique is picked so that distinct 'ReturnWith' handlers for the same
-- type don't catch each other's values.
newReturnWithId :: IO ReturnWithId
newReturnWithId = ReturnWithId <$> newUnique

data ReturnWithWrapper = ReturnWithWrapper !ReturnWithId CallStack Any

instance Show ReturnWithWrapper where
  showsPrec p (ReturnWithWrapper _ cs _)
    = showParen (p > 10)
    $ ("Effectful.ReturnWith.Static.ReturnWithWrapper\n" ++)
    . (prettyCallStack cs ++)

instance Exception ReturnWithWrapper where
  -- See discussion in https://github.com/haskell-effectful/effectful/pull/232.
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

matchReturnWith :: ReturnWithId -> ReturnWithWrapper -> Maybe r
matchReturnWith rid (ReturnWithWrapper rtag _ r)
  | rid == rtag = Just (fromAny r)
  | otherwise = Nothing
