-- | Experimental support for coroutines.
module Effective.Coroutine
  ( Coroutine
  , Status(..)
  , runCoroutine
  , yield
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Data.Function
import qualified Control.Concurrent as C

import Effective.Internal.Env
import Effective.Internal.Has
import Effective.Internal.Monad

data Coroutine o i = forall es r. Coroutine
  { crInput         :: MVar (i, Env es)
  , crState         :: MVar (State o r)
  , crCallerEnvSize :: Int
  }

data Status es o i r
  = Done r
  | Yielded o (i -> Eff es (Status es o i r))

runCoroutine :: Eff (Coroutine o i : es) r -> Eff es (Status es o i r)
runCoroutine (Eff m) = impureEff $ \es -> do
  size    <- sizeEnv es
  mvInput <- newEmptyMVar
  mvState <- newEmptyMVar
  mask $ \restore -> do
    -- Create a worker thread and continue execution there.
    tid <- C.forkIO $ do
      let cr = Coroutine mvInput mvState size
      er <- try $ restore . m =<< unsafeConsEnv cr es
      tryPutMVar mvState (either Failure Success er) >>= \case
        False -> error "unexpected"
        True  -> pure ()
    waitForStatus restore es size tid mvInput mvState

yield :: Coroutine o i :> es => o -> Eff es i
yield o = impureEff $ \es -> mask $ \restore -> do
  Coroutine{..} <- getEnv es
  size <- sizeEnv es
  -- Save local part of the environment as the caller will discard it.
  localEs <- takeLastEnv (size - crCallerEnvSize) es
  -- Pass control to the caller.
  tryPutMVar crState (Yield o) >>= \case
    False -> error "unexpected"
    True  -> do
      (i, callerEs) <- restore $ takeMVar crInput
      -- The caller resumed, reconstruct the local environment. The environment
      -- needs to be replaced since the one we just got might be completely
      -- different to what we had before suspending the computation, e.g. if the
      -- computation was resumed in a different thread.
      unsafeReplaceEnv es =<< unsafeAppendEnv callerEs localEs
      pure i

----------------------------------------
-- Internal

data State o r where
  Failure :: SomeException -> State o r
  Success :: r             -> State o r
  Yield   :: o             -> State o r

waitForStatus
  :: (forall a. IO a -> IO a)
  -> Env es
  -> Int
  -> C.ThreadId
  -> MVar (i, Env es)
  -> MVar (State o r)
  -> IO (Status es o i r)
waitForStatus restore0 es0 size0 tid mvInput mvState = fix $ \loop -> do
  try @SomeException (restore0 $ takeMVar mvState) >>= \case
    Left e            -> throwTo tid e >> loop
    Right (Failure e) -> throwIO e
    Right (Success r) -> Done r      <$ unsafeTrimEnv size0 es0
    Right (Yield o)   -> Yielded o k <$ unsafeTrimEnv size0 es0
  where
    k i = impureEff $ \es -> mask $ \restore -> do
      size <- sizeEnv es
      -- Resume suspended computation with the current environment.
      tryPutMVar mvInput (i, es) >>= \case
        False -> error "unexpected"
        True  -> waitForStatus restore es size tid mvInput mvState
