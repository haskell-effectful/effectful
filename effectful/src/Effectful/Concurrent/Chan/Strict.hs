-- | Lifted "Control.Concurrent.Chan.Strict".
--
-- @since 2.4.0.0
module Effectful.Concurrent.Chan.Strict
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * Chan
  , Chan'
  , newChan'
  , writeChan'
  , readChan'
  , dupChan'
  , getChan'Contents
  , writeList2Chan'
  ) where

import Control.Concurrent.Chan.Strict (Chan')
import Control.Concurrent.Chan.Strict qualified as C

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static

-- | Lifted 'C.newChan''.
newChan' :: Concurrent :> es => Eff es (Chan' a)
newChan' = unsafeEff_ C.newChan'

-- | Lifted 'C.writeChan''.
writeChan' :: Concurrent :> es => Chan' a -> a -> Eff es ()
writeChan' c = unsafeEff_ . C.writeChan' c

-- | Lifted 'C.readChan''.
readChan' :: Concurrent :> es => Chan' a -> Eff es a
readChan' = unsafeEff_ . C.readChan'

-- | Lifted 'C.dupChan''.
dupChan' :: Concurrent :> es => Chan' a -> Eff es (Chan' a)
dupChan' = unsafeEff_ . C.dupChan'

-- | Lifted 'C.getChan'Contents'.
getChan'Contents :: Concurrent :> es => Chan' a -> Eff es [a]
getChan'Contents = unsafeEff_ . C.getChan'Contents

-- | Lifted 'C.writeList2Chan''.
writeList2Chan' :: Concurrent :> es => Chan' a -> [a] -> Eff es ()
writeList2Chan' c = unsafeEff_ . C.writeList2Chan' c
