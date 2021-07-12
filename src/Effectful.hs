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

  -- * 'Effect' handlers

  -- ** Sending operations to the handler
  , send

  -- ** Basic handlers
  , interpret
  , interpretM

  -- ** Derived handlers
  , reinterpret
  , reinterpretM

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
  ) where

import Effectful.Interpreter
import Effectful.Monad
