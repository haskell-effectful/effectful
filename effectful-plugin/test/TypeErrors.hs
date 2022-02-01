{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}

module TypeErrors where

-- $setup
-- >>> default ()
-- >>> :set -package effectful-plugin
-- ...
-- >>> :set -fplugin=Effectful.Plugin
-- >>> :m +Effectful
-- >>> :m +Effectful.State.Static.Local
-- >>> :m +Effectful.Reader.Static
-- >>> :m +Data.Maybe


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- existsKV :: State (Maybe Int) :> es => Eff es Bool
-- existsKV = isJust get
-- :}
-- ...
-- ... Couldn't match expected type ...Eff es Bool... 
-- ... with actual type ...Bool...
-- ...
-- ... Couldn't match expected type...Maybe a0...
-- ... with actual type...Eff es0 s0...
-- ...
missingFmap = ()

