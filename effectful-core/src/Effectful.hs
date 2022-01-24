module Effectful
  ( -- * Introduction
    -- $intro

    -- * The 'Eff' monad
    Eff

    -- ** Effect constraints
  , Effect
  , Dispatch(..)
  , DispatchOf
  , (:>)
  , (:>>)

    -- * Running the 'Eff' monad

    -- ** Pure computations
  , runPureEff

    -- ** Computations with side effects
  , IOE
  , runEff

    -- ** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO

    -- * Re-exports
  , MonadIO(..)
  , MonadUnliftIO(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- $intro
--
-- TODO
--
