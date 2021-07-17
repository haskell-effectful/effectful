module Utils
  ( runShallow
  , runDeep
  ) where

import Effectful.Monad
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
  . evalLocalState () . evalLocalState () . evalLocalState () . evalLocalState ()
  . evalLocalState () . evalLocalState () . evalLocalState () . evalLocalState ()
  . evalLocalState () . evalLocalState ()
