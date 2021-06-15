module Effectful.Monad
  ( -- * The 'Eff' monad
    Eff
  , runEff
  , Effect
  , (:>)

  -- * The IO effect
  , IOE
  , runIOE
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad
