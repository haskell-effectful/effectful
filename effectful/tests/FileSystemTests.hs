module FileSystemTests (fileSystemTests) where

import Data.ByteString.Char8 qualified as BS8
import System.OsPath ((</>))
import System.OsPath qualified as OsPath
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.FileSystem.File.OsPath qualified as F
import Effectful.FileSystem.OsPath
import Effectful.Temporary
import Utils qualified as U

fileSystemTests :: TestTree
fileSystemTests = testGroup "FileSystem"
  [ testCase "OsPath directory operations" test_directoryOperations
  , testCase "OsPath file operations" test_fileOperations
  ]

test_directoryOperations :: Assertion
test_directoryOperations = runEff . runFileSystem . runTemporary $ do
  withSystemTempDirectory "effectful" $ \tmpDirFp -> do
    tmpDir <- OsPath.encodeUtf tmpDirFp
    subDirName <- OsPath.encodeUtf "subdir"
    let subDir = tmpDir </> subDirName
    createDirectory subDir
    subDirExists <- doesDirectoryExist subDir
    U.assertBool "subdirectory was not created" subDirExists
    newDirName <- OsPath.encodeUtf "newdir"
    let newDir = tmpDir </> newDirName
    renameDirectory subDir newDir
    contents <- listDirectory tmpDir
    U.assertEqual "unexpected directory contents" [newDirName] contents
    removeDirectory newDir
    newDirExists <- doesDirectoryExist newDir
    U.assertBool "directory was not removed" (not newDirExists)

test_fileOperations :: Assertion
test_fileOperations = runEff . runFileSystem . runTemporary $ do
  withSystemTempDirectory "effectful" $ \tmpDirFp -> do
    tmpDir <- OsPath.encodeUtf tmpDirFp
    fileName <- OsPath.encodeUtf "test.txt"
    let file = tmpDir </> fileName
    F.writeFile' file (BS8.pack "hello")
    F.appendFile' file (BS8.pack " world")
    contents <- F.readFile' file
    U.assertEqual "unexpected file contents" (BS8.pack "hello world") contents
    size <- getFileSize file
    U.assertEqual "unexpected file size" 11 size
    removeFile file
    fileExists <- doesFileExist file
    U.assertBool "file was not removed" (not fileExists)
