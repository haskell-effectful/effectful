module Effectful.Temporary
  ( Temporary
  , runTemporary
  , withSystemTempFile
  , withSystemTempDirectory
  , withTempFile
  , withTempDirectory
  ) where

import System.IO
import qualified UnliftIO.Temporary as T

import Effectful.Dispatch.Static
import Effectful.Monad

-- | An effect for interacting with temporary files.
data Temporary :: Effect where
  Temporary :: Temporary m r

type instance EffectStyle Temporary = DataA

-- | Run the 'Temporary' effect.
runTemporary :: IOE :> es => Eff (Temporary : es) a -> Eff es a
runTemporary = evalData (DataA Temporary)

-- | Lifted 'T.withSystemTempFile'.
withSystemTempFile
  :: Temporary :> es
  => String
  -- ^ File name template. See 'openTempFile'.
  -> (FilePath -> Handle -> Eff es a)
  -- ^ Callback that can use the file.
  -> Eff es a
withSystemTempFile template action = unsafeUnliftIO $ \unlift -> do
  T.withSystemTempFile template $ \fp handle -> unlift $ action fp handle

-- | Lifted 'T.withSystemTempDirectory'.
withSystemTempDirectory
  :: Temporary :> es
  => String
  -- ^ Directory name template. See 'openTempFile'.
  -> (FilePath -> Eff es a)
  -- ^ Callback that can use the directory.
  -> Eff es a
withSystemTempDirectory template action = unsafeUnliftIO $ \unlift -> do
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
withTempFile tmpDir template action = unsafeUnliftIO $ \unlift -> do
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
withTempDirectory tmpDir template action = unsafeUnliftIO $ \unlift -> do
  T.withTempDirectory tmpDir template $ \fp -> unlift $ action fp
