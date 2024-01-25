-- | Lifted functions from "Data.ByteString.Lazy.Char8" that are related to
-- standard streams.
-- Like the original module, you probably want to import this module qualified
-- to avoid name clashes with the functions provided by "Prelude", e.g.:
--
-- > import Data.ByteString.Lazy.Char8 (ByteString)
-- > import qualified Data.ByteString.Lazy.Char8 as LBS8
-- > import Effectful.Console.ByteString.Lazy (Console)
-- > import qualified Effectful.Console.ByteString.Lazy as Console
--
module Effectful.Console.ByteString.Lazy
  ( -- * Effect
    Console

    -- ** Handlers
  , runConsole

    -- * Operations
  , getContents
  , putStr
  , putStrLn
  , interact
  ) where

import Prelude hiding
  ( getContents
  , interact
  , putStr
  , putStrLn
  )

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8

import Effectful
import Effectful.Console.Effect
import Effectful.Dispatch.Static

-- | Lifted 'LBS8.getContents'.
getContents :: Console :> es => Eff es ByteString
getContents = unsafeEff_ LBS8.getContents

-- | Lifted 'LBS8.putStr'.
putStr :: Console :> es => ByteString -> Eff es ()
putStr = unsafeEff_ . LBS8.putStr

-- | Lifted 'LBS8.putStrLn'.
putStrLn :: Console :> es => ByteString -> Eff es ()
putStrLn = unsafeEff_ . LBS8.putStrLn

-- | Lifted 'LBS8.interact'.
interact :: Console :> es => (ByteString -> ByteString) -> Eff es ()
interact = unsafeEff_ . LBS8.interact
