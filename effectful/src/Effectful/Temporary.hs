module Effectful.Temporary
  ( -- * Effect
    Temporary

    -- ** Handlers
  , runTemporary

    -- ** Operations
  , withSystemTempFile
  , withSystemTempDirectory
  , withTempFile
  , withTempDirectory
  ) where

import System.IO
import UnliftIO.Temporary qualified as T

import Effectful
import Effectful.Dispatch.Static

-- | An effect for interacting with temporary files.
data Temporary :: Effect

type instance DispatchOf Temporary = Static WithSideEffects
data instance StaticRep Temporary = Temporary

-- | Run the 'Temporary' effect.
runTemporary :: IOE :> es => Eff (Temporary : es) a -> Eff es a
runTemporary = evalStaticRep Temporary

-- | Lifted 'T.withSystemTempFile'.
withSystemTempFile
  :: Temporary :> es
  => String
  -- ^ File name template. See 'openTempFile'.
  -> (FilePath -> Handle -> Eff es a)
  -- ^ Callback that can use the file.
  -> Eff es a
withSystemTempFile template action = unsafeSeqUnliftIO $ \unlift -> do
  T.withSystemTempFile template $ \fp handle -> unlift $ action fp handle

-- | Lifted 'T.withSystemTempDirectory'.
withSystemTempDirectory
  :: Temporary :> es
  => String
  -- ^ Directory name template. See 'openTempFile'.
  -> (FilePath -> Eff es a)
  -- ^ Callback that can use the directory.
  -> Eff es a
withSystemTempDirectory template action = unsafeSeqUnliftIO $ \unlift -> do
  T.withSystemTempDirectory template $ \fp -> unlift $ action fp

-- | Lifted 'T.withTempFile'.
withTempFile
  :: Temporary :> es
  => FilePath
  -- ^ Temp dir to create the file in.
  -> String
  -- ^ File name template. See 'openTempFile'.
  -> (FilePath -> Handle -> Eff es a)
  -- ^ Callback that can use the file.
  -> Eff es a
withTempFile tmpDir template action = unsafeSeqUnliftIO $ \unlift -> do
  T.withTempFile tmpDir template $ \fp handle -> unlift $ action fp handle

-- | Lifted 'T.withTempDirectory'.
withTempDirectory
  :: Temporary :> es
  => FilePath
  -- ^ Temp directory to create the directory in.
  -> String
  -- ^ Directory name template. See 'openTempFile'.
  -> (FilePath -> Eff es a)
  -- ^ Callback that can use the directory.
  -> Eff es a
withTempDirectory tmpDir template action = unsafeSeqUnliftIO $ \unlift -> do
  T.withTempDirectory tmpDir template $ \fp -> unlift $ action fp
