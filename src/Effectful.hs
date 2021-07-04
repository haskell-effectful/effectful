module Effectful
  ( -- * The 'Eff' monad
    Eff

  -- ** Effect constraints
  , Effect
  , (:>)

  -- ** Sending effects
  , send

  -- * Running the 'Eff' monad

  -- ** Pure operations
  , runEff

  -- ** Operations with side effects
  , IOE
  , runIOE

  -- * Building 'Effect' handlers

  -- ** Basic handlers
  , interpret
  , interpretM

  -- ** Derived handlers
  , reinterpret
  , reinterpretM
  ) where

import Effectful.Interpreter
import Effectful.Monad
