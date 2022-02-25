module Effectful.FileSystem.Effect
  ( -- * Effect
    FileSystem

    -- ** Handlers
  , runFileSystem
  ) where

import Effectful
import Effectful.Dispatch.Static

-- | An effect for interacting with the filesystem.
data FileSystem :: Effect

type instance DispatchOf FileSystem = Static WithSideEffects
data instance StaticRep FileSystem = FileSystem

-- | Run the 'FileSystem' effect.
runFileSystem :: IOE :> es => Eff (FileSystem : es) a -> Eff es a
runFileSystem = evalStaticRep FileSystem
