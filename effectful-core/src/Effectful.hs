module Effectful
  ( -- * Introduction
    -- $intro

    -- * The 'Eff' monad
    Eff

    -- ** Effect constraints
  , Effect
  , Dispatch(..)
  , DispatchOf
  , (:>)
  , (:>>)

    -- * Running the 'Eff' monad

    -- ** Pure computations
  , runPureEff

    -- ** Computations with side effects
  , runEff
  , IOE

    -- ** Lifting
  , raise
  , subsume
  , inject
  , Subset

    -- ** Unlifting
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO

    -- * Re-exports
  , MonadIO(..)
  , MonadUnliftIO(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- $intro
--
-- Haskell is one of the few programming languages that distinguishes between
-- pure functions and functions that might perform side effects. For example, a
-- function
--
-- @
-- f :: 'Int' -> 'String'
-- @
--
-- can't perform side effects at all, but a function
--
-- @
-- f :: 'Int' -> 'IO' 'String'
-- @
--
-- can perform any side effect. This "all or nothing" approach isn't very
-- satisfactory though, because the vast majority of time we would like to
-- signify that a function can perform /some/ side effects, e.g. only be able to
-- log messages and have a read only access to the logged in user.
--
-- This library provides support for expressing exactly that, using its 'Eff'
-- monad:
--
-- @
-- f :: (Log ':>' es, 'Effectful.Reader.Static.Reader' User ':>' es) => 'Int' -> 'Eff' es 'String'
-- @
--
-- It implements support for extensible effects with both dynamic and static
-- dispatch (for more information about each type consult the documentation in
-- "Effectful.Dispatch.Dynamic" and "Effectful.Dispatch.Static").
--
-- It provides:
--
-- - The 'Eff' monad that tracks effects at the type level.
--
-- - A set of predefined, basic effects such as 'Effectful.Error.Static.Error',
--   'Effectful.Reader.Static.Reader', 'Effectful.State.Static.Local.State' and
--   'Effectful.Writer.Static.Local.Writer'.
--
-- - Utilities for defining new effects and interpreting them, possibly in terms
--   of already existing ones.
--
