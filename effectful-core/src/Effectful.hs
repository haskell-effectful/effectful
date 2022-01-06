module Effectful
  ( -- * The 'Eff' monad
    Eff

  -- ** Effect constraints
  , Effect
  , Dispatch(..)
  , DispatchOf
  , (:>)

  -- * Running the 'Eff' monad

  -- ** Pure computations
  , runPureEff

  -- ** Computations with side effects
  , IOE
  , runEff

  -- *** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO

  -- * 'Effect' handlers

  -- ** Sending operations to the handler
  , send

  -- ** Basic handlers
  , interpret
  , reinterpret

  -- ** Handling local 'Eff' computations
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

import Effectful.Dispatch.Dynamic
import Effectful.Monad
