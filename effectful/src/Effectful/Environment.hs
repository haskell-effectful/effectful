module Effectful.Environment
  ( Environment
  , runEnvironment

    -- * Querying the environment
  , getArgs
  , getProgName
  , getExecutablePath
  , getEnv
  , getEnvironment
  , lookupEnv

    -- * Modifying the environment
  , setEnv
  , unsetEnv
  , withArgs
  , withProgName
  ) where

import qualified System.Environment as E

import Effectful
import Effectful.Dispatch.Static hiding (getEnv)
import Data.Proxy (Proxy(Proxy))

-- | An effect for querying and modifying the system environment.
data Environment :: Effect

type instance DispatchOf Environment = 'Static
data instance StaticRep Environment = Environment
type instance NeedsIO Environment = 'True

-- | Run the 'Environment' effect.
runEnvironment :: IOE :> es => Eff (Environment : es) a -> Eff es a
runEnvironment = evalStaticRep Environment

-- | Lifted 'E.getArgs'.
getArgs :: Environment :> es => Eff es [String]
getArgs = deferIO_ (Proxy @Environment) E.getArgs

-- | Lifted 'E.getEnv'.
getEnv :: Environment :> es => String -> Eff es String
getEnv = deferIO_ (Proxy @Environment) . E.getEnv

-- | Lifted 'E.getEnvironment'.
getEnvironment :: Environment :> es => Eff es [(String, String)]
getEnvironment = deferIO_ (Proxy @Environment) E.getEnvironment

-- | Lifted 'E.getExecutablePath'.
getExecutablePath :: Environment :> es => Eff es FilePath
getExecutablePath = deferIO_ (Proxy @Environment) E.getExecutablePath

-- | Lifted 'E.getProgName'.
getProgName :: Environment :> es => Eff es String
getProgName = deferIO_ (Proxy @Environment) E.getProgName

-- | Lifted 'E.lookupEnv'.
lookupEnv :: Environment :> es => String -> Eff es (Maybe String)
lookupEnv = deferIO_ (Proxy @Environment) . E.lookupEnv

-- | Lifted 'E.setEnv'.
setEnv :: Environment :> es => String -> String -> Eff es ()
setEnv n = deferIO_ (Proxy @Environment) . E.setEnv n

-- | Lifted 'E.unsetEnv'.
unsetEnv :: Environment :> es => String -> Eff es ()
unsetEnv = deferIO_ (Proxy @Environment) . E.unsetEnv

-- | Lifted 'E.withArgs'.
withArgs :: Environment :> es => [String] -> Eff es a -> Eff es a
withArgs = unsafeLiftMapIO . E.withArgs

-- | Lifted 'E.withProgName'.
withProgName :: Environment :> es => String -> Eff es a -> Eff es a
withProgName = unsafeLiftMapIO . E.withProgName
