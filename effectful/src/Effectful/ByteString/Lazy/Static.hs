-- | Lifted "Data.ByteString.Lazy".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString.Lazy (ByteString)
-- > import qualified Data.ByteString.Lazy as LBS
-- > import qualified Effectful.ByteString.Lazy.Static as ELBS
--
module Effectful.ByteString.Lazy.Static
  ( -- * Standard input and output
    getContents
  , putStr
  , interact

    -- * Files
  , readFile
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
  , getContents
  , interact
  , putStr
  , readFile
  , writeFile
  )

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import System.IO (Handle)

import Effectful
import Effectful.Console.Static
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Standard input and output

-- | Lifted 'LBS.getContents'.
getContents :: Console :> es => Eff es ByteString
getContents = unsafeEff_ LBS.getContents

-- | Lifted 'LBS.putStr'.
putStr :: Console :> es => ByteString -> Eff es ()
putStr = unsafeEff_ . LBS.putStr

-- | Lifted 'LBS.interact'.
interact :: Console :> es => (ByteString -> ByteString) -> Eff es ()
interact = unsafeEff_ . LBS.interact

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
