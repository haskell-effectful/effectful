{-# LANGUAGE CPP #-}

-- | Lifted "Data.ByteString.Builder".
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString.Builder (Builder)
-- > import qualified Data.ByteString.Builder as BSB
-- > import qualified Effectful.FileSystem.IO.ByteString.Builder as EBSB
--
module Effectful.FileSystem.IO.ByteString.Builder
  ( -- * Executing Builders
    hPutBuilder
#if MIN_VERSION_bytestring(0,11,2)
  , writeFile
#endif
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Prelude hiding (writeFile)
import System.IO (Handle)

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem

----------------------------------------
-- Executing Builders

-- | Lifted 'BS.Builder.hPutBuilder'.
hPutBuilder :: FileSystem :> es => Handle -> Builder -> Eff es ()
hPutBuilder h = unsafeEff_ . BSB.hPutBuilder h

#if MIN_VERSION_bytestring(0,11,2)
-- | Lifted 'BS.Builder.writeFile'.
writeFile :: FileSystem :> es => FilePath -> Builder -> Eff es ()
writeFile fp = unsafeEff_ . BSB.writeFile fp
#endif
