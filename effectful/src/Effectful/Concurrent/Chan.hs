-- | Lifted "Control.Concurrent.Chan".
module Effectful.Concurrent.Chan
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * Chan
  , Chan
  , newChan
  , writeChan
  , readChan
  , dupChan
  , getChanContents
  , writeList2Chan
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Chan qualified as C

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static

-- | Lifted 'C.newChan'.
newChan :: Concurrent :> es => Eff es (Chan a)
newChan = unsafeEff_ C.newChan

-- | Lifted 'C.writeChan'.
writeChan :: Concurrent :> es => Chan a -> a -> Eff es ()
writeChan c = unsafeEff_ . C.writeChan c

-- | Lifted 'C.readChan'.
readChan :: Concurrent :> es => Chan a -> Eff es a
readChan = unsafeEff_ . C.readChan

-- | Lifted 'C.dupChan'.
dupChan :: Concurrent :> es => Chan a -> Eff es (Chan a)
dupChan = unsafeEff_ . C.dupChan

-- | Lifted 'C.getChanContents'.
getChanContents :: Concurrent :> es => Chan a -> Eff es [a]
getChanContents = unsafeEff_ . C.getChanContents

-- | Lifted 'C.writeList2Chan'.
writeList2Chan :: Concurrent :> es => Chan a -> [a] -> Eff es ()
writeList2Chan c = unsafeEff_ . C.writeList2Chan c
