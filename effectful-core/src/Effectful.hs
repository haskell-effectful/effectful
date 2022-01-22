module Effectful
  ( -- * Overview
    -- $overview

    -- * The 'Eff' monad
    module Effectful.Monad

    -- * Effects

    -- ** Dynamic dispatch
  , module Effectful.Dispatch.Dynamic

    -- ** Static dispatch
    -- $static
  ) where

import Effectful.Dispatch.Dynamic
import Effectful.Monad

-- $overview
-- TODO

-- $static
--
-- Documentation for statically dispatched effects and the API for defining them
-- is available in "Effectful.Dispatch.Static".
