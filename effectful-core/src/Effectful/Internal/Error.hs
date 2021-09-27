{- | Errors thrown by Effectful.

This module is intended for internal use only, and may change without warning
in subsequent releases.

-}
module Effectful.Internal.Error
  ( EffectfulError(..)
  ) where

import Control.Exception

-- | An error thrown by Effectful.
data EffectfulError
  = InvalidNumberOfThreads Int
  | InvalidNumberOfUses Int
  | InvalidUseOfSeqUnlift
  | ExceededNumberOfThreads Int
  | ExceededNumberOfUses Int
  deriving Show

instance Exception EffectfulError where
  displayException (InvalidNumberOfThreads threads) =
    "Invalid number of threads: " ++ show threads
  displayException (InvalidNumberOfUses uses) =
    "Invalid number of uses: " ++ show uses
  displayException (ExceededNumberOfThreads threads) =
    "Number of other threads (" ++ show threads ++ ") permitted to use the " ++
    "unlifting function was exceeded. Please increase the limit or use the " ++
    "unlimited variant."
  displayException (ExceededNumberOfUses uses) =
    "Number of permitted calls (" ++ show uses ++ ") to the unlifting " ++
    "function in other threads was exceeded. Please increase the limit or " ++
    "use the unlimited variant."
  displayException InvalidUseOfSeqUnlift =
    "If you want to use the unlifting function to run Eff operations in " ++
    "multiple threads, have a look at UnliftStrategy (ConcUnlift)."
