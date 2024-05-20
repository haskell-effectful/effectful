-- | Statically dispatched effects.
module Effectful.Dispatch.Static
  ( -- * Introduction
    -- $intro

    -- ** An example
    -- $example

    -- * Low level API
    StaticRep
  , SideEffects(..)
  , MaybeIOE

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
  , unsafeSeqUnliftIO
  , unsafeConcUnliftIO

    -- ** Utils
  , unEff
  , unsafeEff
  , unsafeEff_
  , unsafeLiftMapIO

  -- * Re-exports
  , HasCallStack
  ) where

import GHC.Stack (HasCallStack)

import Effectful.Internal.Env
import Effectful.Internal.Monad

-- $intro
--
-- Unlike dynamically dispatched effects, statically dispatched effects have a
-- single, set interpretation that cannot be changed at runtime, which makes
-- them useful in specific scenarios. For example:
--
-- * If you'd like to ensure that a specific effect will behave in a certain way
--   at all times, using a statically dispatched version is the only way to
--   ensure that.
--
-- * If the effect you're about to define has only one reasonable
--   implementation, it makes a lot of sense to make it statically dispatched.
--
-- __When in doubt, use dynamic dispatch as it's more flexible.__
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
-- When it comes to the dispatch, we also need to signify whether core
-- operations of the effect will perform side effects. Since GHC is not a
-- polygraph, you can lie, though being truthful is recommended 🙂
--
-- >>> type instance DispatchOf Log = Static WithSideEffects
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
-- However, since logging is most often an operation with side effects, in order
-- for this approach to be sound, the function that introduces the @Log@ effect
-- needs to require the 'IOE' effect.
--
-- If you forget to do that, don't worry. As long as the 'DispatchOf' instance
-- was correctly defined to be @'Static' 'WithSideEffects'@, you will get a
-- reminder:
--
-- >>> :{
--  runLog :: Logger -> Eff (Log : es) a -> Eff es a
--  runLog logger = evalStaticRep (Log logger)
-- :}
-- ...
-- ...No instance for ...IOE :> es... arising from a use of ‘evalStaticRep’
-- ...
--
-- Including @'IOE' :> es@ in the context fixes the problem:
--
-- >>> :{
--  runLog :: IOE :> es => Logger -> Eff (Log : es) a -> Eff es a
--  runLog logger = evalStaticRep (Log logger)
-- :}
--
-- In general, whenever any operation of a statically dispatched effect performs
-- side effects using one of the unsafe functions, all functions that introduce
-- this effect need to require the 'IOE' effect (otherwise it would be possible
-- to run it via 'runPureEff').
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
-- /Note:/ the computation must not run its argument in a separate thread,
-- attempting to do so will result in a runtime error.
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' computations.
unsafeLiftMapIO :: HasCallStack => (IO a -> IO b) -> Eff es a -> Eff es b
unsafeLiftMapIO f m = unsafeEff $ \es -> do
  seqUnliftIO es $ \unlift -> f (unlift m)

-- | Create an unlifting function with the 'SeqUnlift' strategy.
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' computations.
unsafeSeqUnliftIO
  :: HasCallStack
  => ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
unsafeSeqUnliftIO k = unsafeEff $ \es -> do
  seqUnliftIO es k

-- | Create an unlifting function with the 'ConcUnlift' strategy.
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' computations.
unsafeConcUnliftIO
  :: HasCallStack
  => Persistence
  -> Limit
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
unsafeConcUnliftIO persistence limit k = unsafeEff $ \es -> do
  concUnliftIO es persistence limit k

-- $setup
-- >>> import Effectful
