{-# LANGUAGE CPP #-}

-- | Lifted "Data.ByteString".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString (ByteString)
-- > import qualified Data.ByteString as BS
-- > import qualified Effectful.FileSystem.IO.ByteString as EBS
--
module Effectful.FileSystem.IO.ByteString
#if MIN_VERSION_bytestring(0,11,2)
  ( -- * Introducing and eliminating ByteStrings
    fromFilePath
  , toFilePath

    -- * Files
  , readFile
#else
  ( -- * Files
    readFile
#endif
  , writeFile
  , appendFile

    -- * I/O with Handles
  , hGetLine
  , hGetContents
  , hGet
  , hGetSome
  , hGetNonBlocking
  , hPut
  , hPutNonBlocking
  , hPutStr
  , hPutStrLn
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Prelude hiding (appendFile, readFile, writeFile)
import System.IO (Handle)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Introducing and eliminating ByteStrings

#if MIN_VERSION_bytestring(0,11,2)
-- | Lifted 'BS.fromFilePath'.
fromFilePath :: FileSystem :> es => FilePath -> Eff es ByteString
fromFilePath = unsafeEff_ . BS.fromFilePath

-- | Lifted 'BS.toFilePath'.
toFilePath :: FileSystem :> es => ByteString -> Eff es FilePath
toFilePath = unsafeEff_ . BS.toFilePath
#endif

----------------------------------------
-- Files

-- | Lifted 'BS8.readFile'.
readFile :: FileSystem :> es => FilePath -> Eff es ByteString
readFile = unsafeEff_ . BS8.readFile

-- | Lifted 'BS8.writeFile'.
writeFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeFile fp = unsafeEff_ . BS8.writeFile fp

-- | Lifted 'BS8.appendFile'.
appendFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
appendFile fp = unsafeEff_ . BS8.appendFile fp

----------------------------------------
-- I/O with Handles

-- | Lifted 'BS8.hGetLine'.
hGetLine :: FileSystem :> es => Handle -> Eff es ByteString
hGetLine = unsafeEff_ . BS8.hGetLine

-- | Lifted 'BS8.hGetContents'.
hGetContents :: FileSystem :> es => Handle -> Eff es ByteString
hGetContents = unsafeEff_ . BS8.hGetContents

-- | Lifted 'BS8.hGet'.
hGet :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGet h = unsafeEff_ . BS8.hGet h

-- | Lifted 'BS8.hGetSome'.
hGetSome :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGetSome h = unsafeEff_ . BS8.hGetSome h

-- | Lifted 'BS8.hGetNonBlocking'.
hGetNonBlocking :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGetNonBlocking h = unsafeEff_ . BS8.hGetNonBlocking h

-- | Lifted 'BS8.hPut'.
hPut :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPut h = unsafeEff_ . BS8.hPut h

-- | Lifted 'BS8.hPutNonBlocking'.
hPutNonBlocking :: FileSystem :> es => Handle -> ByteString -> Eff es ByteString
hPutNonBlocking h = unsafeEff_ . BS8.hPutNonBlocking h

-- | Lifted 'BS8.hPutStr'.
hPutStr :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPutStr h = unsafeEff_ . BS8.hPutStr h

-- | Lifted 'BS8.hPutStrLn'.
hPutStrLn :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPutStrLn h = unsafeEff_ . BS8.hPutStrLn h
