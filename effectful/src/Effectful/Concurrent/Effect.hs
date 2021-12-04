module Effectful.Concurrent.Effect
  ( Concurrent
  , runConcurrent
  ) where

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Run 'Eff' operations asynchronously via the @async@ library.
--
-- /Note:/ thread local state changes in 'Eff' operations run asynchronously
-- will not affect the parent thread.
--
-- /TODO:/ write about 'Concurrent' not respecting scoped operations.
data Concurrent :: Effect where
  Concurrent :: Concurrent m r

runConcurrent :: IOE :> es => Eff (Concurrent : es) a -> Eff es a
runConcurrent = evalEffect (IdE Concurrent)
