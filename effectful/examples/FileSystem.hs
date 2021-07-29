module FileSystem
  ( FileSystemE(..)
  , FsError(..)
  , readFile
  , writeFile
  , runFileSystemIO
  , runFileSystemPure
  ) where

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Prelude hiding (readFile, writeFile)
import qualified Data.Map.Strict as M
import qualified System.IO as IO

import Effectful
import Effectful.Error
import Effectful.State.Local

-- | An effect for reading and writing files.
data FileSystemE :: Effect where
  ReadFile  :: FilePath -> FileSystemE m String
  WriteFile :: FilePath -> String -> FileSystemE m ()

--- | File system error.
newtype FsError = FsError String

----------------------------------------
-- Operations

-- | Read contents of a file.
readFile :: (HasCallStack, FileSystemE :> es) => FilePath -> Eff es String
readFile = send . ReadFile

-- | Write contents to a file.
writeFile :: (HasCallStack, FileSystemE :> es) => FilePath -> String -> Eff es ()
writeFile path = send . WriteFile path

----------------------------------------
-- Handlers

runFileSystemIO
  :: (IOE :> es, ErrorE FsError :> es)
  => Eff (FileSystemE : es) a
  -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile path           -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
  where
    adapt m = liftIO m `catch` \(e::IOException) -> throwError . FsError $ show e

runFileSystemPure
  :: ErrorE FsError :> es
  => M.Map FilePath String
  -> Eff (FileSystemE : es) a
  -> Eff es a
runFileSystemPure fs0 = reinterpret (evalStateE fs0) $ \_ -> \case
  ReadFile path -> gets (M.lookup path) >>= \case
    Just contents -> pure contents
    Nothing       -> throwError . FsError $ "File not found: " ++ show path
  WriteFile path contents -> modify $ M.insert path contents
