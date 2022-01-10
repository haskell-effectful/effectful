-- | Support for access to a mutable value of a particular type.
--
-- The value is thread local. If you want it to be shared between threads, see
-- "Effectful.State.Static.Shared".
--
-- /Note:/ unlike the 'Control.Monad.Trans.State.StateT' monad transformer from
-- the @transformers@ library, the 'State' effect doesn't lose state
-- modifications when an exception is received:
--
-- >>> import qualified Control.Monad.Trans.State.Strict as S
--
-- >>> :{
--   (`S.execStateT` "Hi") . handle (\(_::ErrorCall) -> pure ()) $ do
--     S.modify (++ " there!")
--     error "oops"
-- :}
-- "Hi"
--
-- >>> :{
--   runEff . execState "Hi" . handle (\(_::ErrorCall) -> pure ()) $ do
--     modify (++ " there!")
--     error "oops"
-- :}
-- "Hi there!"
module Effectful.State.Static.Local
  ( State
  , runState
  , evalState
  , execState
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Provide access to a strict (WHNF), thread local, mutable value of type @s@.
data State s :: Effect

type instance DispatchOf (State s) = 'Static
newtype instance StaticRep (State s) = State s

-- | Run a 'State' effect with the given initial state and return the final
-- value along with the final state.
runState
  :: s -- ^ An initial state.
  -> Eff (State s : es) a
  -> Eff es (a, s)
runState s0 m = do
  (a, State s) <- runStaticRep (State s0) m
  pure (a, s)

-- | Run a 'State' effect with the given initial state and return the final
-- value, discarding the final state.
evalState
  :: s -- ^ An initial state.
  -> Eff (State s : es) a
  -> Eff es a -- ^ A return value.
evalState s = evalStaticRep (State s)

-- | Run a 'State' effect with the given initial state and return the final
-- state, discarding the final value.
execState
  :: s -- ^ An initial state.
  -> Eff (State s : es) a
  -> Eff es s
execState s0 m = do
  State s <- execStaticRep (State s0) m
  pure s

-- | Fetch the current value of the state.
get :: State s :> es => Eff es s
get = do
  State s <- getStaticRep
  pure s

-- | Get a function of the current state.
--
-- @'gets' f ≡ f '<$>' 'get'@
gets
  :: State s :> es
  => (s -> a) -- ^ The function to apply to the state.
  -> Eff es a
gets f = f <$> get

-- | Set the current state to the given value.
put :: State s :> es => s -> Eff es ()
put s = putStaticRep (State s)

-- | Apply the function to the current state and return a value.
state
  :: State s :> es
  => (s -> (a, s)) -- ^ The function to modify the state.
  -> Eff es a
state f = stateStaticRep $ \(State s0) -> let (a, s) = f s0 in (a, State s)

-- | Apply the function to the current state.
--
-- @'modify' f ≡ 'state' (\\s -> ((), f s))@
modify
  :: State s :> es
  => (s -> s) -- ^ The function to modify the state.
  -> Eff es ()
modify f = state $ \s -> ((), f s)

-- | Apply the monadic function to the current state and return a value.
stateM
  :: State s :> es
  => (s -> Eff es (a, s)) -- ^ The function to modify the state.
  -> Eff es a
stateM f = stateStaticRepM $ \(State s0) -> do
  (a, s) <- f s0
  pure (a, State s)

-- | Apply the monadic function to the current state.
--
-- @'modifyM' f ≡ 'stateM' (\\s -> ((), ) '<$>' f s)@
modifyM
  :: State s :> es
  => (s -> Eff es s) -- ^ The monadic function to modify the state.
  -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)

-- $setup
-- >>> import Control.Exception (ErrorCall)
-- >>> import Control.Monad.Catch
