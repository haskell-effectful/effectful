module Effectful.Console.Effect
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

type instance DispatchOf Console = Static WithSideEffects
data instance StaticRep Console = Console

-- | Run the 'Console' effect.
runConsole :: IOE :> es => Eff (Console : es) a -> Eff es a
runConsole = evalStaticRep Console
