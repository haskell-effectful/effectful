-- | Lifted functions from "Data.ByteString.Char8" that are related to standard
-- streams.
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString (ByteString)
-- > import qualified Data.ByteString.Char8 as BS8
-- > import Effectful.Console.ByteString (Console)
-- > import qualified Effectful.Console.ByteString as Console
--
module Effectful.Console.ByteString
  ( -- * Effect
    Console

    -- ** Handlers
  , runConsole

    -- * Operations
  , getLine
  , getContents
  , putStr
  , putStrLn
  , interact
  ) where

import Prelude hiding
  ( getContents
  , getLine
  , interact
  , putStr
  , putStrLn
  )

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8

import Effectful
import Effectful.Console.Effect
import Effectful.Dispatch.Static

-- | Lifted 'BS8.getLine'.
getLine :: Console :> es => Eff es ByteString
getLine = unsafeEff_ BS8.getLine

-- | Lifted 'BS8.getContents'.
getContents :: Console :> es => Eff es ByteString
getContents = unsafeEff_ BS8.getContents

-- | Lifted 'BS8.putStr'.
putStr :: Console :> es => ByteString -> Eff es ()
putStr = unsafeEff_ . BS8.putStr

-- | Lifted 'BS8.putStrLn'.
putStrLn :: Console :> es => ByteString -> Eff es ()
putStrLn = unsafeEff_ . BS8.putStrLn

-- | Lifted 'BS8.interact'.
interact :: Console :> es => (ByteString -> ByteString) -> Eff es ()
interact = unsafeEff_ . BS8.interact
