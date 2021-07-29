module Utils
  ( runShallow
  , runDeep
  ) where

import Effectful
import Effectful.State.Dynamic

runShallow :: Eff '[IOE] a -> IO a
runShallow = runEff

runDeep
  :: Eff '[ StateE (), StateE (), StateE (), StateE (), StateE ()
          , StateE (), StateE (), StateE (), StateE (), StateE ()
          , IOE
          ] a
  -> IO a
runDeep = runEff
  . evalLocalStateE () . evalLocalStateE () . evalLocalStateE () . evalLocalStateE ()
  . evalLocalStateE () . evalLocalStateE () . evalLocalStateE () . evalLocalStateE ()
  . evalLocalStateE () . evalLocalStateE ()
