-- | Lifted version of "UnliftIO.IO.File".
module Effectful.FileSystem.IO.File
  ( writeBinaryFile
  , writeBinaryFileAtomic
  , writeBinaryFileDurable
  , writeBinaryFileDurableAtomic

  , withBinaryFile
  , withBinaryFileAtomic
  , withBinaryFileDurable
  , withBinaryFileDurableAtomic

  , ensureFileDurable
  ) where

import Data.ByteString (ByteString)
import System.IO (Handle, IOMode (..))
import qualified UnliftIO.IO.File as U

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem.Effect

-- | Lifted version of 'U.writeBinaryFile'.
writeBinaryFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeBinaryFile path = unsafeEff_ . U.writeBinaryFile path

-- | Lifted version of 'U.writeBinaryFileAtomic'.
writeBinaryFileAtomic :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeBinaryFileAtomic path = unsafeEff_ . U.writeBinaryFileAtomic path

-- | Lifted version of 'U.writeBinaryFileDurable'.
writeBinaryFileDurable :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeBinaryFileDurable path = unsafeEff_ . U.writeBinaryFileDurable path

-- | Lifted version of 'U.writeBinaryFileDurableAtomic'.
writeBinaryFileDurableAtomic :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeBinaryFileDurableAtomic path = unsafeEff_ . U.writeBinaryFileDurableAtomic path

----------------------------------------

-- | Lifted version of 'U.withBinaryFile'.
withBinaryFile
  :: FileSystem :> es => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFile path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  U.withBinaryFile path mode $ unlift . inner

-- | Lifted version of 'U.withBinaryFileAtomic'.
withBinaryFileAtomic
  :: FileSystem :> es => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFileAtomic path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  U.withBinaryFileAtomic path mode $ unlift . inner

-- | Lifted version of 'U.withBinaryFileDurable'.
withBinaryFileDurable
  :: FileSystem :> es => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFileDurable path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  U.withBinaryFileDurable path mode $ unlift . inner

-- | Lifted version of 'U.withBinaryFileDurableAtomic'.
withBinaryFileDurableAtomic
  :: FileSystem :> es => FilePath -> IOMode -> (Handle -> Eff es a) -> Eff es a
withBinaryFileDurableAtomic path mode inner = unsafeSeqUnliftIO $ \unlift -> do
  U.withBinaryFileDurableAtomic path mode $ unlift . inner

----------------------------------------

-- | Lifted version of 'U.ensureFileDurable'.
ensureFileDurable :: FileSystem :> es => FilePath -> Eff es ()
ensureFileDurable = unsafeEff_ . U.ensureFileDurable
