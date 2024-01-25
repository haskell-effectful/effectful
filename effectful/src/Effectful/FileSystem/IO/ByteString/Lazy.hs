-- | Lifted "Data.ByteString.Lazy.Char8".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString.Lazy (ByteString)
-- > import qualified Data.ByteString.Lazy.Char8 as LBS8
-- > import qualified Effectful.FileSystem.IO.ByteString.Lazy as ELBS
--
module Effectful.FileSystem.IO.ByteString.Lazy
  ( -- * Files
    readFile
  , writeFile
  , appendFile

    -- * I/O with Handles
  , hGetContents
  , hGet
  , hGetNonBlocking
  , hPut
  , hPutNonBlocking
  , hPutStr
  , hPutStrLn
  ) where

import Prelude hiding
  ( appendFile
  , readFile
  , writeFile
  )

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import System.IO (Handle)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Files

-- | Lifted 'LBS8.readFile'.
readFile :: FileSystem :> es => FilePath -> Eff es ByteString
readFile = unsafeEff_ . LBS8.readFile

-- | Lifted 'LBS8.writeFile'.
writeFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeFile fp = unsafeEff_ . LBS8.writeFile fp

-- | Lifted 'LBS8.appendFile'.
appendFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
appendFile fp = unsafeEff_ . LBS8.appendFile fp

----------------------------------------
-- I/O with Handles

-- | Lifted 'LBS8.hGetContents'.
hGetContents :: FileSystem :> es => Handle -> Eff es ByteString
hGetContents = unsafeEff_ . LBS8.hGetContents

-- | Lifted 'LBS8.hGet'.
hGet :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGet h = unsafeEff_ . LBS8.hGet h

-- | Lifted 'LBS8.hGetNonBlocking'.
hGetNonBlocking :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGetNonBlocking h = unsafeEff_ . LBS8.hGetNonBlocking h

-- | Lifted 'LBS8.hPut'.
hPut :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPut h = unsafeEff_ . LBS8.hPut h

-- | Lifted 'LBS8.hPutNonBlocking'.
hPutNonBlocking :: FileSystem :> es => Handle -> ByteString -> Eff es ByteString
hPutNonBlocking h = unsafeEff_ . LBS8.hPutNonBlocking h

-- | Lifted 'LBS8.hPutStr'.
hPutStr :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPutStr h = unsafeEff_ . LBS8.hPutStr h

-- | Lifted 'LBS8.hPutStrLn'.
hPutStrLn :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPutStrLn h = unsafeEff_ . LBS8.hPutStrLn h
