module Effectful.STM
  ( STME
  , runSTME
  , atomically
  ) where

import qualified Control.Monad.STM as S

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Run 'S.STM' operations using the @stm@ library.
data STME :: Effect where
  STME :: STME m r

runSTME :: IOE :> es => Eff (STME : es) a -> Eff es a
runSTME = evalEffect (IdE STME)

atomically :: STME :> es => S.STM a -> Eff es a
atomically = unsafeEff_ . S.atomically
