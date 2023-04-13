-- | Lifted functions from "Data.ByteString" that are related to standard
-- streams.
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString (ByteString)
-- > import qualified Data.ByteString as BS
-- > import Effectful.Console.ByteString.Static (Console)
-- > import qualified Effectful.Console.ByteString.Static as Console
--
module Effectful.Console.ByteString.Static
  ( module Effectful.Console.Effect

    -- * Operations
  , getLine
  , getContents
  , putStr
  , interact
  ) where

import Prelude hiding
  ( getContents
  , getLine
  , interact
  , putStr
  )

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Effectful
import Effectful.Console.Effect
import Effectful.Dispatch.Static

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
