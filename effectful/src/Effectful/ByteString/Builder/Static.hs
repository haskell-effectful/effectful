{-# LANGUAGE CPP #-}

-- | Lifted "Data.ByteString.Builder".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString.Builder (Builder)
-- > import qualified Data.ByteString.Builder as BS.Builder
-- > import qualified Effectful.ByteString.Builder.Static as EBS.Builder
--
module Effectful.ByteString.Builder.Static
  ( -- * Executing Builders
    hPutBuilder
#if MIN_VERSION_bytestring(0,11,2)
  , writeFile
#endif
  ) where

import Prelude hiding (writeFile)

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS.Builder
import System.IO (Handle)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Executing Builders

-- | Lifted 'BS.Builder.hPutBuilder'.
hPutBuilder :: FileSystem :> es => Handle -> Builder -> Eff es ()
hPutBuilder h = unsafeEff_ . BS.Builder.hPutBuilder h

#if MIN_VERSION_bytestring(0,11,2)
-- | Lifted 'BS.Builder.writeFile'.
writeFile :: FileSystem :> es => FilePath -> Builder -> Eff es ()
writeFile fp = unsafeEff_ . BS.Builder.writeFile fp
#endif
