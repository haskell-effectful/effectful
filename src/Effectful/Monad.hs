module Effectful.Monad
  ( -- * The 'Eff' monad
    Eff
  , runEff

  -- ** Effect constraints
  , Effect
  , (:>)

  -- * Arbitrary I/O
  , IOE
  , runIOE

  -- ** Unlift strategies
  , UnliftStrategy(..)
  , unliftStrategy
  , withUnliftStrategy
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad
