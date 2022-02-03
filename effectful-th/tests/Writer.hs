{-# LANGUAGE TemplateHaskell #-}
module Writer where

import Effectful.Writer.Dynamic (Writer)
import Effectful.TH

makeSendFunctions ''Writer
