module Effectful.FileSystem.Effect
  ( FileSystem
  , runFileSystem
  ) where

import Effectful.Dispatch.Static
import Effectful.Monad

-- | An effect for interacting with the filesystem.
data FileSystem :: Effect where
  FileSystem :: FileSystem m r

type instance EffectStyle FileSystem = StaticEffect

-- | Run the 'FileSystem' effect.
runFileSystem :: IOE :> es => Eff (FileSystem : es) a -> Eff es a
runFileSystem = evalData (StaticEffect FileSystem)
