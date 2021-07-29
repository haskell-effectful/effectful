-- | The 'Writer' effect.
module Effectful.Writer.Local
  ( WriterE
  , runWriterE
  , execWriterE
  , tell
  , listen
  , listens
  ) where

import Control.Exception

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), thread local, write only value of type
-- @w@.
newtype WriterE w :: Effect where
  WriterE :: w -> WriterE w m r

runWriterE :: Monoid w => Eff (WriterE w : es) a -> Eff es (a, w)
runWriterE m = do
  (a, IdE (WriterE w)) <- runEffect (IdE (WriterE mempty)) m
  pure (a, w)

execWriterE :: Monoid w => Eff (WriterE w : es) a -> Eff es w
execWriterE m = do
  IdE (WriterE w) <- execEffect (IdE (WriterE mempty)) m
  pure w

tell :: (WriterE w :> es, Monoid w) => w -> Eff es ()
tell w = stateEffect $ \(IdE (WriterE w0)) -> ((), IdE (WriterE (w0 <> w)))

listen :: (WriterE w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen m = unsafeEff $ \es -> mask $ \restore -> do
  w0 <- stateEnv es $ \(IdE (WriterE w)) -> (w, IdE (WriterE mempty))
  a <- restore (unEff m es) `onException` merge es w0
  (a, ) <$> merge es w0
  where
    merge es w0 =
      -- If an exception is thrown, restore w0 and keep parts of w1.
      stateEnv es $ \(IdE (WriterE w1)) -> (w1, IdE (WriterE (w0 <> w1)))

listens :: (WriterE w :> es, Monoid w) => (w -> b) -> Eff es a -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
