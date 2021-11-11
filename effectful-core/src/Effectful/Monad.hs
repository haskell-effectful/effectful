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
  , UnliftError(..)

  -- * Re-exports
  , MonadIO(..)
  , MonadUnliftIO(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

import Effectful.Internal.Effect
import Effectful.Internal.Monad
