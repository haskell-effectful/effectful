-- | Lifted "System.File.OsPath".
module Effectful.FileSystem.File.OsPath
  ( -- * Effect
    FileSystem

    -- ** Handlers
  , runFileSystem

    -- * Files
  , IOMode (..)
  , Handle
  , openBinaryFile
  , withFile
  , withBinaryFile
  , withFile'
  , withBinaryFile'
  , readFile
  , readFile'
  , writeFile
  , writeFile'
  , appendFile
  , appendFile'
  , openFile
  , openExistingFile
  , openTempFile
  , openBinaryTempFile
  , openTempFileWithDefaultPermissions
  , openBinaryTempFileWithDefaultPermissions
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Prelude hiding (appendFile, readFile, writeFile)
import System.File.OsPath qualified as F
import System.IO (Handle, IOMode (..))
import System.OsPath (OsPath, OsString)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem.Effect

-- | Lifted 'F.openBinaryFile'.
openBinaryFile :: FileSystem :> es => OsPath -> IOMode -> Eff es Handle
openBinaryFile path = unsafeEff_ . F.openBinaryFile path

-- | Lifted 'F.withFile'.
withFile
  :: FileSystem :> es
  => OsPath
  -> IOMode
  -> (Handle -> Eff es a)
  -> Eff es a
withFile path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  F.withFile path mode $ unlift . inner

-- | Lifted 'F.withBinaryFile'.
withBinaryFile
  :: FileSystem :> es
  => OsPath
  -> IOMode
  -> (Handle -> Eff es a)
  -> Eff es a
withBinaryFile path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  F.withBinaryFile path mode $ unlift . inner

-- | Lifted 'F.withFile''.
withFile'
  :: FileSystem :> es
  => OsPath
  -> IOMode
  -> (Handle -> Eff es a)
  -> Eff es a
withFile' path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  F.withFile' path mode $ unlift . inner

-- | Lifted 'F.withBinaryFile''.
withBinaryFile'
  :: FileSystem :> es
  => OsPath
  -> IOMode
  -> (Handle -> Eff es a)
  -> Eff es a
withBinaryFile' path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  F.withBinaryFile' path mode $ unlift . inner

-- | Lifted 'F.readFile'.
readFile :: FileSystem :> es => OsPath -> Eff es BSL.ByteString
readFile = unsafeEff_ . F.readFile

-- | Lifted 'F.readFile''.
readFile' :: FileSystem :> es => OsPath -> Eff es ByteString
readFile' = unsafeEff_ . F.readFile'

-- | Lifted 'F.writeFile'.
writeFile :: FileSystem :> es => OsPath -> BSL.ByteString -> Eff es ()
writeFile path = unsafeEff_ . F.writeFile path

-- | Lifted 'F.writeFile''.
writeFile' :: FileSystem :> es => OsPath -> ByteString -> Eff es ()
writeFile' path = unsafeEff_ . F.writeFile' path

-- | Lifted 'F.appendFile'.
appendFile :: FileSystem :> es => OsPath -> BSL.ByteString -> Eff es ()
appendFile path = unsafeEff_ . F.appendFile path

-- | Lifted 'F.appendFile''.
appendFile' :: FileSystem :> es => OsPath -> ByteString -> Eff es ()
appendFile' path = unsafeEff_ . F.appendFile' path

-- | Lifted 'F.openFile'.
openFile :: FileSystem :> es => OsPath -> IOMode -> Eff es Handle
openFile path = unsafeEff_ . F.openFile path

-- | Lifted 'F.openExistingFile'.
openExistingFile :: FileSystem :> es => OsPath -> IOMode -> Eff es Handle
openExistingFile path = unsafeEff_ . F.openExistingFile path

-- | Lifted 'F.openTempFile'.
openTempFile
  :: FileSystem :> es
  => OsPath
  -> OsString
  -> Eff es (OsPath, Handle)
openTempFile dir = unsafeEff_ . F.openTempFile dir

-- | Lifted 'F.openBinaryTempFile'.
openBinaryTempFile
  :: FileSystem :> es
  => OsPath
  -> OsString
  -> Eff es (OsPath, Handle)
openBinaryTempFile dir = unsafeEff_ . F.openBinaryTempFile dir

-- | Lifted 'F.openTempFileWithDefaultPermissions'.
openTempFileWithDefaultPermissions
  :: FileSystem :> es
  => OsPath
  -> OsString
  -> Eff es (OsPath, Handle)
openTempFileWithDefaultPermissions dir =
  unsafeEff_ . F.openTempFileWithDefaultPermissions dir

-- | Lifted 'F.openBinaryTempFileWithDefaultPermissions'.
openBinaryTempFileWithDefaultPermissions
  :: FileSystem :> es
  => OsPath
  -> OsString
  -> Eff es (OsPath, Handle)
openBinaryTempFileWithDefaultPermissions dir =
  unsafeEff_ . F.openBinaryTempFileWithDefaultPermissions dir
