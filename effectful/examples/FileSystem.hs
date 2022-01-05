module FileSystem
  ( FileSystem(..)
  , FsError(..)
  , readFile
  , writeFile
  , runFileSystemIO
  , runFileSystemPure
  ) where

import Control.Exception (IOException)
import Control.Monad.Catch
import Prelude hiding (readFile, writeFile)
import qualified Data.Map.Strict as M
import qualified System.IO as IO

import Effectful
import Effectful.Error
import Effectful.State.Local

-- | An effect for reading and writing files.
data FileSystem :: Effect where
  ReadFile  :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance EffectStyle FileSystem = DynamicEffect

--- | File system error.
newtype FsError = FsError String

----------------------------------------
-- Operations

-- | Read contents of a file.
readFile :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es String
readFile = send . ReadFile

-- | Write contents to a file.
writeFile :: (HasCallStack, FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile path = send . WriteFile path

----------------------------------------
-- Handlers

runFileSystemIO
  :: (IOE :> es, Error FsError :> es)
  => Eff (FileSystem : es) a
  -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile path           -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
  where
    adapt m = liftIO m `catch` \(e::IOException) -> throwError . FsError $ show e

runFileSystemPure
  :: Error FsError :> es
  => M.Map FilePath String
  -> Eff (FileSystem : es) a
  -> Eff es a
runFileSystemPure fs0 = reinterpret (evalState fs0) $ \_ -> \case
  ReadFile path -> gets (M.lookup path) >>= \case
    Just contents -> pure contents
    Nothing       -> throwError . FsError $ "File not found: " ++ show path
  WriteFile path contents -> modify $ M.insert path contents
