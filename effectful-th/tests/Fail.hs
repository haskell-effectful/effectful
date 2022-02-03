{-# LANGUAGE TemplateHaskell #-}
module Fail where

import Prelude hiding (fail)

import Effectful.Fail (Fail)
import Effectful.TH

makeSendFunctions ''Fail
