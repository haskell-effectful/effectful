-- | Statically dispatched effects.
module Effectful.Dispatch.Static
  ( -- * Introduction
    -- $intro

    -- ** An example
    -- $example

    -- * Low level API
    StaticRep

    -- ** Extending the environment
  , runStaticRep
  , evalStaticRep
  , execStaticRep

    -- ** Data retrieval and update
  , getStaticRep
  , putStaticRep
  , stateStaticRep
  , stateStaticRepM
  , localStaticRep

    -- ** Unlifts
  , seqUnliftIO
  , concUnliftIO

    -- ** Utils
  , unEff
  , unsafeEff
  , unsafeEff_
  , unsafeLiftMapIO
  , unsafeUnliftIO

    -- * Primitive API
  , Env
  , Relinker(..)
  , dummyRelinker

    -- ** Representation of effects
  , EffectRep

    -- ** Operations
  , emptyEnv
  , cloneEnv
  , forkEnv
  , sizeEnv
  , checkSizeEnv

    -- ** Extending and shrinking
  , unsafeConsEnv
  , unsafeTailEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv
  ) where

import Effectful.Internal.Env
import Effectful.Internal.Monad

-- $intro
--
-- Unlike dynamically dispatched effects, statically dispatched effects have a
-- single, set interpretation that cannot be changed at runtime. It's worth
-- noting that this doesn't make them worse, just applicable in different
-- scenarios. For example:
--
-- * If you'd like to ensure that a specific effect will behave in a certain way
--   at all times, using a statically dispatched version is the only way to
--   ensure that.
--
-- * If the effect you're about to define has only one reasonable implemenation,
--   it makes a lot of sense to make it statically dispatched.
--
-- Statically dispatched effects also perform better than dynamically dispatched
-- ones, because their operations are implemented as standard top level
-- functions, so they can be inlined by the compiler if appropriate.
--

-- $example
--
-- Let's say that there exists a logging library whose functionality we'd like
-- to turn into an effect. Its @Logger@ data type (after simplification) is
-- represented in the following way:
--
-- >>> data Logger = Logger { logMessage :: String -> IO () }
--
-- Because the @Logger@ type itself allows customization of how messages are
-- logged, it is an excellent candidate to be turned into a statically
-- dispatched effect.
--
-- Such effect is represented by an empty data type of kind 'Effectful.Effect':
--
-- >>> data Log :: Effect
--
-- >>> type instance DispatchOf Log = 'Static
--
-- The environment of 'Eff' will hold the data type that represents the
-- effect. It is defined by the appropriate instance of the 'StaticRep' data
-- family:
--
-- >>> newtype instance StaticRep Log = Log Logger
--
-- /Note:/ all operations of a statically dispatched effect will have a
-- read/write access to this data type as long as they can see its constructors,
-- hence it's best not to export them from the module that defines the effect.
--
-- The logging operation can be defined as follows:
--
-- >>> :{
--  log :: (IOE :> es, Log :> es) => String -> Eff es ()
--  log msg = do
--    Log logger <- getStaticRep
--    liftIO $ logMessage logger msg
-- :}
--
-- That works, but has an unfortunate consequence: in order to use the @log@
-- operation the 'IOE' effect needs to be in scope! This is bad, because we're
-- trying to limit (ideally, fully eliminate) the need to have the full power of
-- 'IO' available in the application code. The solution is to use one of the
-- escape hatches that allow unrestricted access to the internal representation
-- of 'Eff':
--
-- >>> :{
--  log :: Log :> es => String -> Eff es ()
--  log msg = do
--    Log logger <- getStaticRep
--    unsafeEff_ $ logMessage logger msg
-- :}
--
-- However, in order for this approach to be sound, the function that introduces
-- the @Log@ effect needs to require 'IOE':
--
-- >>> :{
--  runLog :: IOE :> es => Logger -> Eff (Log : es) a -> Eff es a
--  runLog logger = evalStaticRep (Log logger)
-- :}
--
-- In general, whenever any operation of a static effect introduces potential
-- side effects using one of the unsafe functions, all functions that introduce
-- this effect need to require the 'IOE' effect.
--
-- __If you forget, that's on you, the compiler will not complain.__
--
-- Now we can use the newly defined effect to log messages:
--
-- >>> dummyLogger = Logger { logMessage = \_ -> pure () }
--
-- >>> stdoutLogger = Logger { logMessage = putStrLn }
--
-- >>> :{
--   action = do
--     log "Computing things..."
--     log "Sleeping..."
--     log "Computing more things..."
--     pure True
-- :}
--
-- >>> :t action
-- action :: (Log :> es) => Eff es Bool
--
-- >>> runEff . runLog stdoutLogger $ action
-- Computing things...
-- Sleeping...
-- Computing more things...
-- True
--
-- >>> runEff . runLog dummyLogger $ action
-- True
--

-- | Utility for lifting 'IO' computations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' es a -> 'Eff' es b@
--
-- This function is __unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - The 'IO' computation must not run its argument in a separate thread, but
--   it's not checked anywhere.
unsafeLiftMapIO :: (IO a -> IO b) -> Eff es a -> Eff es b
unsafeLiftMapIO f m = unsafeEff $ \es -> f (unEff m es)

-- | Utility for running 'Eff' computations locally in the 'IO' monad.
--
-- This function is __unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - Unlifted 'Eff' computations must not be run in a thread distinct from the
--   caller of 'unsafeUnliftIO', but it's not checked anywhere.
unsafeUnliftIO :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
unsafeUnliftIO k = unsafeEff $ \es -> k (`unEff` es)

-- $setup
-- >>> import Effectful
