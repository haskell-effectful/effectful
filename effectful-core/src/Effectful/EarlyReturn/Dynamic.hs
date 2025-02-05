-- | Dynamically dispatched EarlyReturn effect.
--
-- The @EarlyReturn@ effect allows you to define a block from which you can
-- return early. Implemented as an exception.
--
-- Unlike many handlers which will be run near the entrypoint of a program, you
-- will most likely run your @EarlyReturn@ effect handler in the block you want
-- to return early from. As example of the former, with the handler in @main@:
--
-- >>> :{
-- import Control.Monad              (when)
-- import Effectful                  (Eff, IOE, liftIO, runEff, (:>))
-- import Effectful.Dispatch.Dynamic (HasCallStack)
-- :}
--
-- >>> :{
-- early1 :: forall es. (HasCallStack, IOE :> es, EarlyReturn Bool :> es) => Int -> Eff es Bool
-- early1 n = do
--   when (n > 10) $ do
--     returnWith True
--   pure False
-- :}
-- 
-- >>> :{
-- f1 :: forall es. (HasCallStack, IOE :> es, EarlyReturn Bool :> es) => Int -> Eff es Bool
-- f1 x = do
--   y <- early1 x
--   liftIO $ putStrLn $ "in f1, y=" <> show y
--   pure y
-- :}
--
-- >>> :{
-- main1 :: IO ()
-- main1 = do
--   x <- runEff . runEarlyReturn $ f1 20
--   print x
-- :}
--
-- >>> main1
-- True
--
-- Note @"in f1, y=True"@ was not printed, the handler caught our @EarlyReturn@
-- and we end up back in @main1@.
--
-- Now with our handler running in the block we want to be able to return early
-- from:
--
-- >>> :{
-- early2 :: forall es. (HasCallStack, IOE :> es) => Int -> Eff es Bool
-- early2 x = runEarlyReturn $ do
--   when (x > 10) $ do
--     returnWith True
--   pure False
-- :}
--
-- >>> :{
-- f2 :: forall es a. (HasCallStack, IOE :> es) => Int -> Eff es Bool
-- f2 x = do
--   y <- early2 x
--   liftIO $ putStrLn $ "in f2, y=" <> show y
--   pure y
-- :}
--
-- >>> :{
-- main2 :: IO ()
-- main2 = do
--   y <- runEff $ f2 20
--   print y
-- :}
--
-- >>> main2
-- in f2, y=True
-- True
--
-- This way the EarlyReturn effect is handled in @early2@ and we see @"in f2,
-- y=True"@.
--
module Effectful.EarlyReturn.Dynamic
  ( -- * Effect
    EarlyReturn(..)

    -- ** Handlers
  , runEarlyReturn
  , runEarlyReturnEither

    -- ** Operations
  , returnWith
  ) where

import           Effectful                  (Dispatch (..), DispatchOf, Eff,
                                             Effect, (:>))
import           Effectful.Dispatch.Dynamic (HasCallStack, reinterpret_, send)
import           Effectful.Error.Dynamic    (runErrorNoCallStack, throwError_)

data EarlyReturn r :: Effect where
  ReturnWith :: r -> EarlyReturn r m a

type instance DispatchOf (EarlyReturn r) = Dynamic

returnWith
  :: (HasCallStack, EarlyReturn r :> es)
  => r
  -> Eff es a
returnWith = send . ReturnWith

runEarlyReturnEither :: Eff (EarlyReturn r : es) a -> Eff es (Either r a)
runEarlyReturnEither = reinterpret_ runErrorNoCallStack $ \case
  ReturnWith r -> throwError_ r

runEarlyReturn :: Eff (EarlyReturn a : es) a -> Eff es a
runEarlyReturn = fmap (either id id) . runEarlyReturnEither

