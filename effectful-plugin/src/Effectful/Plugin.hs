module Effectful.Plugin (plugin) where

import Effectful.Plugin.Internal (Plugin, makePlugin)

plugin :: Plugin
plugin = makePlugin [("effectful", "Effectful.Internal.Effect", ":>")]
