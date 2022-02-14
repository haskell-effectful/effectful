{-# LANGUAGE TemplateHaskell #-}
module Reader where

import Effectful.Reader.Dynamic (Reader)
import Effectful.TH

makeEffect ''Reader
