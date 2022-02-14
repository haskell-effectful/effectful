-- These extensions are needed for the definition of the effect.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-- These extensions are needed for the generation of the functions.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module MyReader where

import Effectful (Dispatch(Dynamic), DispatchOf)
import Effectful.TH

data Reader r :: Effect where
  Ask   :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

type instance DispatchOf (Reader r) = 'Dynamic

makeEffect ''Reader
