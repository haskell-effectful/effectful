-- | An effect for reading/writing to standard streams.
-- This module does intentionally provide no operations for the effect; Instead
-- those are implemented in more specfic module like:
--
--  * "Effectful.Console.ByteString.Static"
--  * "Effectful.Console.ByteString.Lazy.Static"
--
-- The contents of this module is re-exported from those, so you probably want
-- to import one of those instead of this one.
module Effectful.Console.Static
  ( -- * Effect
    Console

    -- ** Handlers
  , runConsole
  ) where

import Effectful
import Effectful.Dispatch.Static

-- | An effect for reading from/writing to 'System.IO.stdin', 'System.IO.stdout'
-- or 'System.IO.stderr'.
data Console :: Effect

type instance DispatchOf Console = 'Static 'WithSideEffects
data instance StaticRep Console = Console

runConsole :: IOE :> es => Eff (Console ': es) a -> Eff es a
runConsole = evalStaticRep Console
