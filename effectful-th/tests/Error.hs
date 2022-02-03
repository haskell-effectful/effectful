{-# LANGUAGE TemplateHaskell #-}
module Error where

import Effectful.Error.Dynamic (Error)
import Effectful.TH

makeSendFunctions ''Error
