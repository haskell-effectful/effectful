module Effectful
  ( -- * The 'Eff' monad
    Eff

  -- ** Effect constraints
  , Effect
  , (:>)

  -- * Running the 'Eff' monad

  -- ** Pure operations
  , runPureEff

  -- ** Operations with side effects
  , IOE
  , runEff

  -- *** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO
  , UnliftError(..)

  -- * 'Effect' handlers

  -- ** Sending operations to the handler
  , send

  -- ** Basic handlers
  , interpret
  , reinterpret

  -- ** Handling local 'Eff' operations
  , LocalEnv

  -- *** Unlifts
  , localSeqUnlift
  , localSeqUnliftIO
  , localUnlift
  , localUnliftIO

  -- *** Lifts
  , withLiftMap
  , withLiftMapIO

  -- *** Bidirectional lifts
  , localLiftUnlift
  , localLiftUnliftIO

  -- * Re-exports
  , MonadIO(..)
  , MonadUnliftIO(..)
  ) where

import Effectful.Handler
import Effectful.Monad
