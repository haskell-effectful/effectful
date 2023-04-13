-- | Lifted "Data.ByteString.Lazy".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString.Lazy (ByteString)
-- > import qualified Data.ByteString.Lazy as LBS
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
  ) where

import Prelude hiding
  ( appendFile
  , readFile
  , writeFile
  )

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import System.IO (Handle)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Files

-- | Lifted 'LBS.readFile'.
readFile :: FileSystem :> es => FilePath -> Eff es ByteString
readFile = unsafeEff_ . LBS.readFile

-- | Lifted 'LBS.writeFile'.
writeFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeFile fp = unsafeEff_ . LBS.writeFile fp

-- | Lifted 'LBS.appendFile'.
appendFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
appendFile fp = unsafeEff_ . LBS.appendFile fp

----------------------------------------
-- I/O with Handles

-- | Lifted 'LBS.hGetContents'.
hGetContents :: FileSystem :> es => Handle -> Eff es ByteString
hGetContents = unsafeEff_ . LBS.hGetContents

-- | Lifted 'LBS.hGet'.
hGet :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGet h = unsafeEff_ . LBS.hGet h

-- | Lifted 'LBS.hGetNonBlocking'.
hGetNonBlocking :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGetNonBlocking h = unsafeEff_ . LBS.hGetNonBlocking h

-- | Lifted 'LBS.hPut'.
hPut :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPut h = unsafeEff_ . LBS.hPut h

-- | Lifted 'LBS.hPutNonBlocking'.
hPutNonBlocking :: FileSystem :> es => Handle -> ByteString -> Eff es ByteString
hPutNonBlocking h = unsafeEff_ . LBS.hPutNonBlocking h
