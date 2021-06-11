module FileSystem
  ( FileSystem
  , FsError(..)
  , readFile
  , writeFile
  , runFileSystemIO
  , runFileSystemPure
  ) where

import Control.Monad.Catch
import Prelude hiding (readFile, writeFile)
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified System.IO as IO

import Effective
import Effective.Error

-- | Effect for reading and writing files.
data FileSystem = forall fs. FileSystem
  { _fs        :: fs
  , _readFile  :: forall es. Error FsError :> es => fs -> FilePath -> Eff es String
  , _writeFile :: forall es. Error FsError :> es => fs -> FilePath -> String -> Eff es fs
  }

-- | File system error.
newtype FsError = FsError String
  deriving Show
instance Exception FsError

----------------------------------------
-- Operations

-- | Read contents of a file.
readFile :: (FileSystem :> es, Error FsError :> es) => FilePath -> Eff es String
readFile path = readerEffectM $ \FileSystem{..} -> _readFile _fs path

-- | Write contents to a file.
writeFile :: (FileSystem :> es, Error FsError :> es) => FilePath -> String -> Eff es ()
writeFile path contents = stateEffectM $ \FileSystem{..} -> do
  fs <- _writeFile _fs path contents
  pure ((), FileSystem { _fs = fs, .. })

----------------------------------------
-- Handlers

runFileSystemIO :: Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = evalEffect $ FileSystem
  { _fs        = ()
  , _readFile  = \() path          -> wrap $ IO.readFile path
  , _writeFile = \() path contents -> wrap $ IO.writeFile path contents
  }
  where
    wrap m = impureEff_ m `catch` \(e::E.IOException) -> throwError . FsError $ show e

runFileSystemPure :: M.Map FilePath String -> Eff (FileSystem : es) a -> Eff es a
runFileSystemPure fs0 = evalEffect $ FileSystem
  { _fs        = fs0
  , _readFile  = \fs path -> case path `M.lookup` fs of
      Just contents -> pure contents
      Nothing       -> throwError . FsError $ "File not found: " ++ show path
  , _writeFile = \fs path contents -> pure $ M.insert path contents fs
  }
