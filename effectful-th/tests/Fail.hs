{-# LANGUAGE TemplateHaskell #-}
module Fail where

import Effectful.Fail (Fail)
import Effectful.TH

makeSendFunctions ''Fail
