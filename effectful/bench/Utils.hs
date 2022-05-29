module Utils
  ( runShallow
  , runDeep
  ) where

import Effectful
import Effectful.State.Dynamic

runShallow :: Eff '[IOE] a -> IO a
runShallow = runEff

runDeep
  :: Eff '[ State (), State (), State (), State (), State ()
          , State (), State (), State (), State (), State ()
          , IOE
          ] a
  -> IO a
runDeep = runEff
  . evalStateLocal () . evalStateLocal () . evalStateLocal () . evalStateLocal ()
  . evalStateLocal () . evalStateLocal () . evalStateLocal () . evalStateLocal ()
  . evalStateLocal () . evalStateLocal ()
