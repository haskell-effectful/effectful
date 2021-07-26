module Effectful.Monad
  ( -- * The 'Eff' monad
    Eff
  , runPureEff

  -- ** Effect constraints
  , Effect
  , (:>)

  -- * Arbitrary I/O
  , IOE
  , runEff

  -- ** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad
