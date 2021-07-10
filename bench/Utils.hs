module Utils
  ( runShallow
  , runDeep
  ) where

import Effectful
import Effectful.State.Dynamic

runShallow :: Eff '[IOE] a -> IO a
runShallow = runIOE

runDeep
  :: Eff '[ State (), State (), State (), State (), State ()
          , State (), State (), State (), State (), State ()
          , IOE
          ] a
  -> IO a
runDeep = runIOE
  . evalState () . evalState () . evalState () . evalState () . evalState ()
  . evalState () . evalState () . evalState () . evalState () . evalState ()
