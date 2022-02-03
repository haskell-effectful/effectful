{-# LANGUAGE TemplateHaskell #-}
module State where

import Effectful.State.Dynamic (State)
import Effectful.TH

makeSendFunctions ''State
