-- | Lifted "System.Directory.OsPath".
module Effectful.FileSystem.OsPath
  ( -- * Effect
    FileSystem

    -- ** Handlers
  , runFileSystem

    -- * Actions on directories
  , createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , removePathForcibly
  , renameDirectory
  , listDirectory
  , getDirectoryContents

    -- ** Current working directory
  , getCurrentDirectory
  , setCurrentDirectory
  , withCurrentDirectory

    -- * Pre-defined directories
  , getHomeDirectory
  , getXdgDirectory
  , getXdgDirectoryList
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory

    -- * Actions on files
  , removeFile
  , renameFile
  , renamePath
  , copyFile
  , copyFileWithMetadata
  , getFileSize
  , canonicalizePath
  , makeAbsolute
  , makeRelativeToCurrentDirectory

    -- * Existence tests
  , doesPathExist
  , doesFileExist
  , doesDirectoryExist
  , findExecutable
  , findExecutables
  , findExecutablesInDirectories
  , findFile
  , findFiles
  , findFileWith
  , findFilesWith

    -- * Symbolic links
  , createFileLink
  , createDirectoryLink
  , removeDirectoryLink
  , pathIsSymbolicLink
  , getSymbolicLinkTarget

    -- * Permissions
  , getPermissions
  , setPermissions
  , copyPermissions

    -- * Timestamps
  , getAccessTime
  , getModificationTime
  , setAccessTime
  , setModificationTime

    -- * Re-exports

    -- ** Pre-defined directories
  , D.XdgDirectory(..)
  , D.XdgDirectoryList(..)

    -- ** Existence tests
  , D.exeExtension

    -- ** Permissions
  , D.Permissions
  , D.emptyPermissions
  , D.readable
  , D.writable
  , D.executable
  , D.searchable
  , D.setOwnerReadable
  , D.setOwnerWritable
  , D.setOwnerExecutable
  , D.setOwnerSearchable
  ) where

import Data.Time (UTCTime)
import System.Directory.OsPath qualified as D
import System.OsPath (OsPath, OsString)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem.Effect

----------------------------------------
-- Actions on directories

-- | Lifted 'System.Directory.OsPath.createDirectory'.
createDirectory :: FileSystem :> es => OsPath -> Eff es ()
createDirectory = unsafeEff_ . D.createDirectory

-- | Lifted 'System.Directory.OsPath.createDirectoryIfMissing'.
createDirectoryIfMissing :: FileSystem :> es => Bool -> OsPath -> Eff es ()
createDirectoryIfMissing doCreateParents =
  unsafeEff_ . D.createDirectoryIfMissing doCreateParents

-- | Lifted 'System.Directory.OsPath.removeDirectory'.
removeDirectory :: FileSystem :> es => OsPath -> Eff es ()
removeDirectory = unsafeEff_ . D.removeDirectory

-- | Lifted 'System.Directory.OsPath.removeDirectoryRecursive'.
removeDirectoryRecursive :: FileSystem :> es => OsPath -> Eff es ()
removeDirectoryRecursive = unsafeEff_ . D.removeDirectoryRecursive

-- | Lifted 'System.Directory.OsPath.removePathForcibly'.
removePathForcibly :: FileSystem :> es => OsPath -> Eff es ()
removePathForcibly = unsafeEff_ . D.removePathForcibly

-- | Lifted 'System.Directory.OsPath.renameDirectory'.
renameDirectory :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
renameDirectory old = unsafeEff_ . D.renameDirectory old

-- | Lifted 'System.Directory.OsPath.listDirectory'.
listDirectory :: FileSystem :> es => OsPath -> Eff es [OsPath]
listDirectory = unsafeEff_ . D.listDirectory

-- | Lifted 'System.Directory.OsPath.getDirectoryContents'.
getDirectoryContents :: FileSystem :> es => OsPath -> Eff es [OsPath]
getDirectoryContents = unsafeEff_ . D.getDirectoryContents

----------------------------------------
-- Current working directory

-- | Lifted 'System.Directory.OsPath.getCurrentDirectory'.
getCurrentDirectory :: FileSystem :> es => Eff es OsPath
getCurrentDirectory = unsafeEff_ D.getCurrentDirectory

-- | Lifted 'System.Directory.OsPath.setCurrentDirectory'.
setCurrentDirectory :: FileSystem :> es => OsPath -> Eff es ()
setCurrentDirectory = unsafeEff_ . D.setCurrentDirectory

-- | Lifted 'System.Directory.OsPath.withCurrentDirectory'.
withCurrentDirectory :: FileSystem :> es => OsPath -> Eff es a -> Eff es a
withCurrentDirectory path = unsafeLiftMapIO (D.withCurrentDirectory path)

----------------------------------------
-- Pre-defined directories

-- | Lifted 'System.Directory.OsPath.getHomeDirectory'.
getHomeDirectory :: FileSystem :> es => Eff es OsPath
getHomeDirectory = unsafeEff_ D.getHomeDirectory

-- | Lifted 'System.Directory.OsPath.getXdgDirectory'.
getXdgDirectory
  :: FileSystem :> es
  => D.XdgDirectory
  -> OsPath
  -> Eff es OsPath
getXdgDirectory xdgDir = unsafeEff_ . D.getXdgDirectory xdgDir

-- | Lifted 'System.Directory.OsPath.getXdgDirectoryList'.
getXdgDirectoryList
  :: FileSystem :> es
  => D.XdgDirectoryList
  -> Eff es [OsPath]
getXdgDirectoryList = unsafeEff_ . D.getXdgDirectoryList

-- | Lifted 'System.Directory.OsPath.getAppUserDataDirectory'.
getAppUserDataDirectory :: FileSystem :> es => OsPath -> Eff es OsPath
getAppUserDataDirectory = unsafeEff_ . D.getAppUserDataDirectory

-- | Lifted 'System.Directory.OsPath.getUserDocumentsDirectory'.
getUserDocumentsDirectory :: FileSystem :> es => Eff es OsPath
getUserDocumentsDirectory = unsafeEff_ D.getUserDocumentsDirectory

-- | Lifted 'System.Directory.OsPath.getTemporaryDirectory'.
getTemporaryDirectory :: FileSystem :> es => Eff es OsPath
getTemporaryDirectory = unsafeEff_ D.getTemporaryDirectory

----------------------------------------
-- Actions on files

-- | Lifted 'System.Directory.OsPath.removeFile'.
removeFile :: FileSystem :> es => OsPath -> Eff es ()
removeFile = unsafeEff_ . D.removeFile

-- | Lifted 'System.Directory.OsPath.renameFile'.
renameFile :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
renameFile old = unsafeEff_ . D.renameFile old

-- | Lifted 'System.Directory.OsPath.renamePath'.
renamePath :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
renamePath old = unsafeEff_ . D.renamePath old

-- | Lifted 'System.Directory.OsPath.copyFile'.
copyFile :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
copyFile src = unsafeEff_ . D.copyFile src

-- | Lifted 'System.Directory.OsPath.copyFileWithMetadata'.
copyFileWithMetadata :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
copyFileWithMetadata src = unsafeEff_ . D.copyFileWithMetadata src

-- | Lifted 'System.Directory.OsPath.getFileSize'.
getFileSize :: FileSystem :> es => OsPath -> Eff es Integer
getFileSize = unsafeEff_ . D.getFileSize

-- | Lifted 'System.Directory.OsPath.canonicalizePath'.
canonicalizePath :: FileSystem :> es => OsPath -> Eff es OsPath
canonicalizePath = unsafeEff_ . D.canonicalizePath

-- | Lifted 'System.Directory.OsPath.makeAbsolute'.
makeAbsolute :: FileSystem :> es => OsPath -> Eff es OsPath
makeAbsolute = unsafeEff_ . D.makeAbsolute

-- | Lifted 'System.Directory.OsPath.makeRelativeToCurrentDirectory'.
makeRelativeToCurrentDirectory
  :: FileSystem :> es
  => OsPath
  -> Eff es OsPath
makeRelativeToCurrentDirectory = unsafeEff_ . D.makeRelativeToCurrentDirectory

----------------------------------------
-- Existence tests

-- | Lifted 'System.Directory.OsPath.doesPathExist'.
doesPathExist :: FileSystem :> es => OsPath -> Eff es Bool
doesPathExist = unsafeEff_ . D.doesPathExist

-- | Lifted 'System.Directory.OsPath.doesFileExist'.
doesFileExist :: FileSystem :> es => OsPath -> Eff es Bool
doesFileExist = unsafeEff_ . D.doesFileExist

-- | Lifted 'System.Directory.OsPath.doesDirectoryExist'.
doesDirectoryExist :: FileSystem :> es => OsPath -> Eff es Bool
doesDirectoryExist = unsafeEff_ . D.doesDirectoryExist

-- | Lifted 'System.Directory.OsPath.findExecutable'.
findExecutable :: FileSystem :> es => OsString -> Eff es (Maybe OsPath)
findExecutable = unsafeEff_ . D.findExecutable

-- | Lifted 'System.Directory.OsPath.findExecutables'.
findExecutables :: FileSystem :> es => OsString -> Eff es [OsPath]
findExecutables = unsafeEff_ . D.findExecutables

-- | Lifted 'System.Directory.OsPath.findExecutablesInDirectories'.
findExecutablesInDirectories
  :: FileSystem :> es
  => [OsPath]
  -> OsString
  -> Eff es [OsPath]
findExecutablesInDirectories dirs =
  unsafeEff_ . D.findExecutablesInDirectories dirs

-- | Lifted 'System.Directory.OsPath.findFile'.
findFile :: FileSystem :> es => [OsPath] -> OsString -> Eff es (Maybe OsPath)
findFile dirs = unsafeEff_ . D.findFile dirs

-- | Lifted 'System.Directory.OsPath.findFiles'.
findFiles :: FileSystem :> es => [OsPath] -> OsString -> Eff es [OsPath]
findFiles dirs = unsafeEff_ . D.findFiles dirs

-- | Lifted 'System.Directory.OsPath.findFileWith'.
findFileWith
  :: FileSystem :> es
  => (OsPath -> Eff es Bool)
  -> [OsPath]
  -> OsString
  -> Eff es (Maybe OsPath)
findFileWith p dirs n = unsafeSeqUnliftIO $ \unlift -> do
  D.findFileWith (unlift . p) dirs n

-- | Lifted 'System.Directory.OsPath.findFilesWith'.
findFilesWith
  :: FileSystem :> es
  => (OsPath -> Eff es Bool)
  -> [OsPath]
  -> OsString
  -> Eff es [OsPath]
findFilesWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> do
  D.findFilesWith (unlift . p) dirs ns

----------------------------------------
-- Symbolic links

-- | Lifted 'System.Directory.OsPath.createFileLink'.
createFileLink :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
createFileLink target = unsafeEff_ . D.createFileLink target

-- | Lifted 'System.Directory.OsPath.createDirectoryLink'.
createDirectoryLink :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
createDirectoryLink target = unsafeEff_ . D.createDirectoryLink target

-- | Lifted 'System.Directory.OsPath.removeDirectoryLink'.
removeDirectoryLink :: FileSystem :> es => OsPath -> Eff es ()
removeDirectoryLink = unsafeEff_ . D.removeDirectoryLink

-- | Lifted 'System.Directory.OsPath.pathIsSymbolicLink'.
pathIsSymbolicLink :: FileSystem :> es => OsPath -> Eff es Bool
pathIsSymbolicLink = unsafeEff_ . D.pathIsSymbolicLink

-- | Lifted 'System.Directory.OsPath.getSymbolicLinkTarget'.
getSymbolicLinkTarget :: FileSystem :> es => OsPath -> Eff es OsPath
getSymbolicLinkTarget = unsafeEff_ . D.getSymbolicLinkTarget

----------------------------------------
-- Permissions

-- | Lifted 'System.Directory.OsPath.getPermissions'.
getPermissions :: FileSystem :> es => OsPath -> Eff es D.Permissions
getPermissions = unsafeEff_ . D.getPermissions

-- | Lifted 'System.Directory.OsPath.setPermissions'.
setPermissions :: FileSystem :> es => OsPath -> D.Permissions -> Eff es ()
setPermissions path = unsafeEff_ . D.setPermissions path

-- | Lifted 'System.Directory.OsPath.copyPermissions'.
copyPermissions :: FileSystem :> es => OsPath -> OsPath -> Eff es ()
copyPermissions src = unsafeEff_ . D.copyPermissions src

----------------------------------------
-- Timestamps

-- | Lifted 'System.Directory.OsPath.getAccessTime'.
getAccessTime :: FileSystem :> es => OsPath -> Eff es UTCTime
getAccessTime = unsafeEff_ . D.getAccessTime

-- | Lifted 'System.Directory.OsPath.getModificationTime'.
getModificationTime :: FileSystem :> es => OsPath -> Eff es UTCTime
getModificationTime = unsafeEff_ . D.getModificationTime

-- | Lifted 'System.Directory.OsPath.setAccessTime'.
setAccessTime :: FileSystem :> es => OsPath -> UTCTime -> Eff es ()
setAccessTime path = unsafeEff_ . D.setAccessTime path

-- | Lifted 'System.Directory.OsPath.setModificationTime'.
setModificationTime :: FileSystem :> es => OsPath -> UTCTime -> Eff es ()
setModificationTime path = unsafeEff_ . D.setModificationTime path
