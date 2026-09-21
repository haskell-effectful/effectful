module Effectful.Environment
  ( -- * Effect
    Environment

    -- ** Handlers
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

import System.Environment qualified as E

import Effectful
import Effectful.Dispatch.Static

-- | An effect for querying and modifying the system environment.
data Environment :: Effect

type instance DispatchOf Environment = Static WithSideEffects
data instance StaticRep Environment = Environment

-- | Run the t'Environment' effect.
runEnvironment :: (HasCallStack, IOE :> es) => Eff (Environment : es) a -> Eff es a
runEnvironment = evalStaticRep Environment

-- | Lifted 'System.Environment.getArgs'.
getArgs :: Environment :> es => Eff es [String]
getArgs = unsafeEff_ E.getArgs

-- | Lifted 'System.Environment.getEnv'.
getEnv :: Environment :> es => String -> Eff es String
getEnv = unsafeEff_ . E.getEnv

-- | Lifted 'System.Environment.getEnvironment'.
getEnvironment :: Environment :> es => Eff es [(String, String)]
getEnvironment = unsafeEff_ E.getEnvironment

-- | Lifted 'System.Environment.getExecutablePath'.
getExecutablePath :: Environment :> es => Eff es FilePath
getExecutablePath = unsafeEff_ E.getExecutablePath

-- | Lifted 'System.Environment.getProgName'.
getProgName :: Environment :> es => Eff es String
getProgName = unsafeEff_ E.getProgName

-- | Lifted 'System.Environment.lookupEnv'.
lookupEnv :: Environment :> es => String -> Eff es (Maybe String)
lookupEnv = unsafeEff_ . E.lookupEnv

-- | Lifted 'System.Environment.setEnv'.
setEnv :: Environment :> es => String -> String -> Eff es ()
setEnv n = unsafeEff_ . E.setEnv n

-- | Lifted 'System.Environment.unsetEnv'.
unsetEnv :: Environment :> es => String -> Eff es ()
unsetEnv = unsafeEff_ . E.unsetEnv

-- | Lifted 'System.Environment.withArgs'.
withArgs :: Environment :> es => [String] -> Eff es a -> Eff es a
withArgs = unsafeLiftMapIO . E.withArgs

-- | Lifted 'System.Environment.withProgName'.
withProgName :: Environment :> es => String -> Eff es a -> Eff es a
withProgName = unsafeLiftMapIO . E.withProgName
