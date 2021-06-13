{-# OPTIONS_HADDOCK not-home #-}
-- | Miscellaneous functions.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Utils
  ( ppCallStack
  ) where

import GHC.Stack.Types

-- | Pretty print a position from a 'CallStack'.
ppCallStack :: (String, SrcLoc) -> String
ppCallStack (fun, SrcLoc {..}) = fun ++ " at " ++ srcLocFile
  ++ ":" ++ show srcLocStartLine ++ ":" ++ show srcLocStartCol
  ++ " in " ++ srcLocPackage ++ ":" ++ srcLocModule
