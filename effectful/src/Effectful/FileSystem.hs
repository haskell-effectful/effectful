module Effectful.FileSystem
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
import System.Directory qualified as D

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem.Effect

----------------------------------------
-- Actions on directories

-- | Lifted 'D.createDirectory'.
createDirectory :: FileSystem :> es => FilePath -> Eff es ()
createDirectory = unsafeEff_ . D.createDirectory

-- | Lifted 'D.createDirectoryIfMissing'.
createDirectoryIfMissing :: FileSystem :> es => Bool -> FilePath -> Eff es ()
createDirectoryIfMissing doCreateParents =
  unsafeEff_ . D.createDirectoryIfMissing doCreateParents

-- | Lifted 'D.removeDirectory'.
removeDirectory :: FileSystem :> es => FilePath -> Eff es ()
removeDirectory = unsafeEff_ . D.removeDirectory

-- | Lifted 'D.removeDirectoryRecursive'.
removeDirectoryRecursive :: FileSystem :> es => FilePath -> Eff es ()
removeDirectoryRecursive = unsafeEff_ . D.removeDirectoryRecursive

-- | Lifted 'D.removePathForcibly'.
removePathForcibly :: FileSystem :> es => FilePath -> Eff es ()
removePathForcibly = unsafeEff_ . D.removePathForcibly

-- | Lifted 'D.renameDirectory'.
renameDirectory :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
renameDirectory old = unsafeEff_ . D.renameDirectory old

-- | Lifted 'D.listDirectory'.
listDirectory :: FileSystem :> es => FilePath -> Eff es [FilePath]
listDirectory = unsafeEff_ . D.listDirectory

-- | Lifted 'D.getDirectoryContents'.
getDirectoryContents :: FileSystem :> es => FilePath -> Eff es [FilePath]
getDirectoryContents = unsafeEff_ . D.getDirectoryContents

----------------------------------------
-- Current working directory

-- | Lifted 'D.getCurrentDirectory'.
getCurrentDirectory :: FileSystem :> es => Eff es FilePath
getCurrentDirectory = unsafeEff_ D.getCurrentDirectory

-- | Lifted 'D.setCurrentDirectory'.
setCurrentDirectory :: FileSystem :> es => FilePath -> Eff es ()
setCurrentDirectory = unsafeEff_ . D.setCurrentDirectory

-- | Lifted 'D.withCurrentDirectory'.
withCurrentDirectory :: FileSystem :> es => FilePath -> Eff es a -> Eff es a
withCurrentDirectory path = unsafeLiftMapIO (D.withCurrentDirectory path)

----------------------------------------
-- Pre-defined directories

-- | Lifted 'D.getHomeDirectory'.
getHomeDirectory :: FileSystem :> es => Eff es FilePath
getHomeDirectory = unsafeEff_ D.getHomeDirectory

-- | Lifted 'D.getXdgDirectory'.
getXdgDirectory
  :: FileSystem :> es
  => D.XdgDirectory
  -> FilePath
  -> Eff es FilePath
getXdgDirectory xdgDir = unsafeEff_ . D.getXdgDirectory xdgDir

-- | Lifted 'D.getXdgDirectoryList'.
getXdgDirectoryList
  :: FileSystem :> es
  => D.XdgDirectoryList
  -> Eff es [FilePath]
getXdgDirectoryList = unsafeEff_ . D.getXdgDirectoryList

-- | Lifted 'D.getAppUserDataDirectory'.
getAppUserDataDirectory :: FileSystem :> es => FilePath -> Eff es FilePath
getAppUserDataDirectory = unsafeEff_ . D.getAppUserDataDirectory

-- | Lifted 'D.getUserDocumentsDirectory'.
getUserDocumentsDirectory :: FileSystem :> es => Eff es FilePath
getUserDocumentsDirectory = unsafeEff_ D.getUserDocumentsDirectory

-- | Lifted 'D.getTemporaryDirectory'.
getTemporaryDirectory :: FileSystem :> es => Eff es FilePath
getTemporaryDirectory = unsafeEff_ D.getTemporaryDirectory

----------------------------------------
-- Actions on files

-- | Lifted 'D.removeFile'.
removeFile :: FileSystem :> es => FilePath -> Eff es ()
removeFile = unsafeEff_ . D.removeFile

-- | Lifted 'D.renameFile'.
renameFile :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
renameFile old = unsafeEff_ . D.renameFile old

-- | Lifted 'D.renamePath'.
renamePath :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
renamePath old = unsafeEff_ . D.renamePath old

-- | Lifted 'D.copyFile'.
copyFile :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
copyFile src = unsafeEff_ . D.copyFile src

-- | Lifted 'D.copyFileWithMetadata'.
copyFileWithMetadata :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
copyFileWithMetadata src = unsafeEff_ . D.copyFileWithMetadata src

-- | Lifted 'D.getFileSize'.
getFileSize :: FileSystem :> es => FilePath -> Eff es Integer
getFileSize = unsafeEff_ . D.getFileSize

-- | Lifted 'D.canonicalizePath'.
canonicalizePath :: FileSystem :> es => FilePath -> Eff es FilePath
canonicalizePath = unsafeEff_ . D.canonicalizePath

-- | Lifted 'D.makeAbsolute'.
makeAbsolute :: FileSystem :> es => FilePath -> Eff es FilePath
makeAbsolute = unsafeEff_ . D.makeAbsolute

-- | Lifted 'D.makeRelativeToCurrentDirectory'.
makeRelativeToCurrentDirectory
  :: FileSystem :> es
  => FilePath
  -> Eff es FilePath
makeRelativeToCurrentDirectory = unsafeEff_ . D.makeRelativeToCurrentDirectory

----------------------------------------
-- Existence tests

-- | Lifted 'D.doesPathExist'.
doesPathExist :: FileSystem :> es => FilePath -> Eff es Bool
doesPathExist = unsafeEff_ . D.doesPathExist

-- | Lifted 'D.doesFileExist'.
doesFileExist :: FileSystem :> es => FilePath -> Eff es Bool
doesFileExist = unsafeEff_ . D.doesFileExist

-- | Lifted 'D.doesDirectoryExist'.
doesDirectoryExist :: FileSystem :> es => FilePath -> Eff es Bool
doesDirectoryExist = unsafeEff_ . D.doesDirectoryExist

-- | Lifted 'D.findExecutable'.
findExecutable :: FileSystem :> es => String -> Eff es (Maybe FilePath)
findExecutable = unsafeEff_ . D.findExecutable

-- | Lifted 'D.findExecutables'.
findExecutables :: FileSystem :> es => String -> Eff es [FilePath]
findExecutables = unsafeEff_ . D.findExecutables

-- | Lifted 'D.findExecutablesInDirectories'.
findExecutablesInDirectories
  :: FileSystem :> es
  => [FilePath]
  -> String
  -> Eff es [FilePath]
findExecutablesInDirectories dirs =
  unsafeEff_ . D.findExecutablesInDirectories dirs

-- | Lifted 'D.findFile'.
findFile :: FileSystem :> es => [FilePath] -> String -> Eff es (Maybe FilePath)
findFile dirs = unsafeEff_ . D.findFile dirs

-- | Lifted 'D.findFiles'.
findFiles :: FileSystem :> es => [FilePath] -> String -> Eff es [FilePath]
findFiles dirs = unsafeEff_ . D.findFiles dirs

-- | Lifted 'D.findFileWith'.
findFileWith
  :: FileSystem :> es
  => (FilePath -> Eff es Bool)
  -> [FilePath]
  -> String
  -> Eff es (Maybe FilePath)
findFileWith p dirs n = unsafeSeqUnliftIO $ \unlift -> do
  D.findFileWith (unlift . p) dirs n

-- | Lifted 'D.findFilesWith'.
findFilesWith
  :: FileSystem :> es
  => (FilePath -> Eff es Bool)
  -> [FilePath]
  -> String
  -> Eff es [FilePath]
findFilesWith p dirs ns = unsafeSeqUnliftIO $ \unlift -> do
  D.findFilesWith (unlift . p) dirs ns

----------------------------------------
-- Symbolic links

-- | Lifted 'D.createFileLink'.
createFileLink :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
createFileLink target = unsafeEff_ . D.createFileLink target

-- | Lifted 'D.createDirectoryLink'.
createDirectoryLink :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
createDirectoryLink target = unsafeEff_ . D.createDirectoryLink target

-- | Lifted 'D.removeDirectoryLink'.
removeDirectoryLink :: FileSystem :> es => FilePath -> Eff es ()
removeDirectoryLink = unsafeEff_ . D.removeDirectoryLink

-- | Lifted 'D.pathIsSymbolicLink'.
pathIsSymbolicLink :: FileSystem :> es => FilePath -> Eff es Bool
pathIsSymbolicLink = unsafeEff_ . D.pathIsSymbolicLink

-- | Lifted 'D.getSymbolicLinkTarget'.
getSymbolicLinkTarget :: FileSystem :> es => FilePath -> Eff es FilePath
getSymbolicLinkTarget = unsafeEff_ . D.getSymbolicLinkTarget

----------------------------------------
-- Permissions

-- | Lifted 'D.getPermissions'.
getPermissions :: FileSystem :> es => FilePath -> Eff es D.Permissions
getPermissions = unsafeEff_ . D.getPermissions

-- | Lifted 'D.setPermissions'.
setPermissions :: FileSystem :> es => FilePath -> D.Permissions -> Eff es ()
setPermissions path = unsafeEff_ . D.setPermissions path

-- | Lifted 'D.copyPermissions'.
copyPermissions :: FileSystem :> es => FilePath -> FilePath -> Eff es ()
copyPermissions src = unsafeEff_ . D.copyPermissions src

----------------------------------------
-- Timestamps

-- | Lifted 'D.getAccessTime'.
getAccessTime :: FileSystem :> es => FilePath -> Eff es UTCTime
getAccessTime = unsafeEff_ . D.getAccessTime

-- | Lifted 'D.getModificationTime'.
getModificationTime :: FileSystem :> es => FilePath -> Eff es UTCTime
getModificationTime = unsafeEff_ . D.getModificationTime

-- | Lifted 'D.setAccessTime'.
setAccessTime :: FileSystem :> es => FilePath -> UTCTime -> Eff es ()
setAccessTime path = unsafeEff_ . D.setAccessTime path

-- | Lifted 'D.setModificationTime'.
setModificationTime :: FileSystem :> es => FilePath -> UTCTime -> Eff es ()
setModificationTime path = unsafeEff_ . D.setModificationTime path
