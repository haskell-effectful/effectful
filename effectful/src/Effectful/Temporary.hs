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

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | An effect for interacting with temporary files.
data Temporary :: Effect where
  Temporary :: Temporary m r

-- | Run the 'Temporary' effect.
runTemporary :: IOE :> es => Eff (Temporary : es) a -> Eff es a
runTemporary = evalEffect (IdE Temporary)

-- | Lifted 'T.withSystemTempFile'.
withSystemTempFile
  :: Temporary :> es
  => String
  -- ^ File name template. See 'openTempFile'.
  -> (FilePath -> Handle -> Eff es a)
  -- ^ Callback that can use the file.
  -> Eff es a
withSystemTempFile template action = unsafeEff $ \es -> do
  T.withSystemTempFile template $ \fp handle -> unEff (action fp handle) es

-- | Lifted 'T.withSystemTempDirectory'.
withSystemTempDirectory
  :: Temporary :> es
  => String
  -- ^ Directory name template. See 'openTempFile'.
  -> (FilePath -> Eff es a)
  -- ^ Callback that can use the directory.
  -> Eff es a
withSystemTempDirectory template action = unsafeEff $ \es -> do
  T.withSystemTempDirectory template $ \fp -> unEff (action fp) es

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
withTempFile tmpDir template action = unsafeEff $ \es -> do
  T.withTempFile tmpDir template $ \fp handle -> unEff (action fp handle) es

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
withTempDirectory tmpDir template action = unsafeEff $ \es -> do
  T.withTempDirectory tmpDir template $ \fp -> unEff (action fp) es
