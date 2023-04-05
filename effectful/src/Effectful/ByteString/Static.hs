{-# LANGUAGE CPP#-}

-- | Lifted "Data.ByteString".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString (ByteString)
-- > import qualified Data.ByteString as BS
-- > import qualified Effectful.ByteString.Static as EBS
--
module Effectful.ByteString.Static
#if MIN_VERSION_bytestring(0,11,2)
  ( -- * Introducing and eliminating ByteStrings
    fromFilePath
  , toFilePath

    -- * Standard input and output
  , getLine
#else
  ( -- * Standard input and output
    getLine
#endif
  , getContents
  , putStr
  , interact

    -- * Files
  , readFile
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
  ) where

import Prelude hiding
  ( appendFile
  , getContents
  , getLine
  , interact
  , putStr
  , readFile
  , writeFile
  )

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.IO (Handle)

import Effectful
import Effectful.Console.Static
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Introducing and eliminating ByteStrings

#if MIN_VERSION_bytestring(0,11,2)
-- | Lifted 'BS.fromFilePath'.
fromFilePath :: IOE :> es => FilePath -> Eff es ByteString
fromFilePath = unsafeEff_ . BS.fromFilePath

-- | Lifted 'BS.toFilePath'.
toFilePath :: IOE :> es => ByteString -> Eff es FilePath
toFilePath = unsafeEff_ . BS.toFilePath
#endif

----------------------------------------
-- Standard input and output

-- | Lifted 'BS.getLine'.
getLine :: Console :> es => Eff es ByteString
getLine = unsafeEff_ BS.getLine

-- | Lifted 'BS.getContents'.
getContents :: Console :> es => Eff es ByteString
getContents = unsafeEff_ BS.getContents

-- | Lifted 'BS.putStr'.
putStr :: Console :> es => ByteString -> Eff es ()
putStr = unsafeEff_ . BS.putStr

-- | Lifted 'BS.interact'.
interact :: Console :> es => (ByteString -> ByteString) -> Eff es ()
interact = unsafeEff_ . BS.interact

----------------------------------------
-- Files

-- | Lifted 'BS.readFile'.
readFile :: FileSystem :> es => FilePath -> Eff es ByteString
readFile = unsafeEff_ . BS.readFile

-- | Lifted 'BS.writeFile'.
writeFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
writeFile fp = unsafeEff_ . BS.writeFile fp

-- | Lifted 'BS.appendFile'.
appendFile :: FileSystem :> es => FilePath -> ByteString -> Eff es ()
appendFile fp = unsafeEff_ . BS.appendFile fp

----------------------------------------
-- I/O with Handles

-- | Lifted 'BS.hGetLine'.
hGetLine :: FileSystem :> es => Handle -> Eff es ByteString
hGetLine = unsafeEff_ . BS.hGetLine

-- | Lifted 'BS.hGetContents'.
hGetContents :: FileSystem :> es => Handle -> Eff es ByteString
hGetContents = unsafeEff_ . BS.hGetContents

-- | Lifted 'BS.hGet'.
hGet :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGet h = unsafeEff_ . BS.hGet h

-- | Lifted 'BS.hGetSome'.
hGetSome :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGetSome h = unsafeEff_ . BS.hGetSome h

-- | Lifted 'BS.hGetNonBlocking'.
hGetNonBlocking :: FileSystem :> es => Handle -> Int -> Eff es ByteString
hGetNonBlocking h = unsafeEff_ . BS.hGetNonBlocking h

-- | Lifted 'BS.hPut'.
hPut :: FileSystem :> es => Handle -> ByteString -> Eff es ()
hPut h = unsafeEff_ . BS.hPut h

-- | Lifted 'BS.hPutNonBlocking'.
hPutNonBlocking :: FileSystem :> es => Handle -> ByteString -> Eff es ByteString
hPutNonBlocking h = unsafeEff_ . BS.hPutNonBlocking h
