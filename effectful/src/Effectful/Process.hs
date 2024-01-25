{-# LANGUAGE CPP #-}
-- | Lifted "System.Process".
module Effectful.Process
  ( -- * Effect
    Process

    -- ** Handlers
  , runProcess

    -- * Running sub-processes
  , createProcess
  , createProcess_
  , P.shell
  , P.proc
  , P.CreateProcess(..)
  , P.CmdSpec(..)
  , P.StdStream(..)
  , P.ProcessHandle

    -- ** Simpler functions for common tasks
  , callProcess
  , callCommand
  , spawnProcess
  , spawnCommand
  , readCreateProcess
  , readProcess
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  , withCreateProcess
  , cleanupProcess

    -- ** Related utilities
  , P.showCommandForUser
  , P.Pid
  , getPid
#if MIN_VERSION_process(1,6,12)
  , getCurrentPid
#endif

    -- * Process completion
  , waitForProcess
  , getProcessExitCode
  , terminateProcess
  , interruptProcessGroupOf

    -- * Interprocess communication
  , createPipe
  , createPipeFd
  ) where

import System.Exit (ExitCode)
import System.IO (Handle)
import System.Posix.Internals (FD)
import System.Process qualified as P

import Effectful
import Effectful.Dispatch.Static

-- | An effect for running child processes using the @process@ library.
data Process :: Effect

type instance DispatchOf Process = Static WithSideEffects
data instance StaticRep Process = Process

runProcess :: IOE :> es => Eff (Process : es) a -> Eff es a
runProcess = evalStaticRep Process

----------------------------------------
-- Running sub-processes

-- | Lifted 'P.createProcess'.
createProcess
  :: Process :> es
  => P.CreateProcess
  -> Eff es (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
createProcess = unsafeEff_ . P.createProcess

-- | Lifted 'P.createProcess_'.
createProcess_
  :: Process :> es
  => String
  -> P.CreateProcess
  -> Eff es (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
createProcess_ msg = unsafeEff_ . P.createProcess_ msg

----------------------------------------
-- Simpler functions for common tasks

-- | Lifted 'P.callProcess'.
callProcess :: Process :> es => FilePath -> [String] -> Eff es ()
callProcess fp = unsafeEff_ . P.callProcess fp

-- | Lifted 'P.callCommand'.
callCommand :: Process :> es => String -> Eff es ()
callCommand = unsafeEff_ . P.callCommand

-- | Lifted 'P.spawnProcess'.
spawnProcess :: Process :> es => FilePath -> [String] -> Eff es P.ProcessHandle
spawnProcess fp = unsafeEff_ . P.spawnProcess fp

-- | Lifted 'P.spawnCommand'.
spawnCommand :: Process :> es => String -> Eff es P.ProcessHandle
spawnCommand = unsafeEff_ . P.spawnCommand

-- | Lifted 'P.readCreateProcess'.
readCreateProcess :: Process :> es => P.CreateProcess -> String -> Eff es String
readCreateProcess cp = unsafeEff_ . P.readCreateProcess cp

-- | Lifted 'P.readProcess'.
readProcess :: Process :> es => FilePath -> [String] -> String -> Eff es String
readProcess fp args = unsafeEff_ . P.readProcess fp args

-- | Lifted 'P.readCreateProcessWithExitCode'.
readCreateProcessWithExitCode
  :: Process :> es
  => P.CreateProcess
  -> String
  -> Eff es (ExitCode, String, String)
readCreateProcessWithExitCode cp = unsafeEff_ . P.readCreateProcessWithExitCode cp

-- | Lifted 'P.readProcessWithExitCode'.
readProcessWithExitCode
  :: Process :> es
  => FilePath
  -> [String]
  -> String
  -> Eff es (ExitCode, String, String)
readProcessWithExitCode fp args = unsafeEff_ . P.readProcessWithExitCode fp args

-- | Lifted 'P.withCreateProcess'.
withCreateProcess
  :: Process :> es
  => P.CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> P.ProcessHandle -> Eff es a)
  -> Eff es a
withCreateProcess cp cb = unsafeSeqUnliftIO $ \unlift -> do
  P.withCreateProcess cp $ \inh outh errh ph -> unlift $ cb inh outh errh ph

-- | Lifted 'P.cleanupProcess'.
cleanupProcess
  :: Process :> es
  => (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
  -> Eff es ()
cleanupProcess = unsafeEff_ . P.cleanupProcess

----------------------------------------
-- Related utilities

-- | Lifted 'P.getPid'.
getPid :: Process :> es => P.ProcessHandle -> Eff es (Maybe P.Pid)
getPid = unsafeEff_ . P.getPid

#if MIN_VERSION_process(1,6,12)
-- | Lifted 'P.getCurrentPid'.
getCurrentPid :: Process :> es => Eff es P.Pid
getCurrentPid = unsafeEff_ P.getCurrentPid
#endif

----------------------------------------
-- Process completion

-- | Lifted 'P.waitForProcess'.
waitForProcess :: Process :> es => P.ProcessHandle -> Eff es ExitCode
waitForProcess = unsafeEff_ . P.waitForProcess

-- | Lifted 'P.getProcessExitCode'.
getProcessExitCode :: Process :> es => P.ProcessHandle -> Eff es (Maybe ExitCode)
getProcessExitCode = unsafeEff_ . P.getProcessExitCode

-- | Lifted 'P.terminateProcess'.
terminateProcess :: Process :> es => P.ProcessHandle -> Eff es ()
terminateProcess = unsafeEff_ . P.terminateProcess

-- | Lifted 'P.interruptProcessGroupOf'.
interruptProcessGroupOf :: Process :> es => P.ProcessHandle -> Eff es ()
interruptProcessGroupOf = unsafeEff_ . P.interruptProcessGroupOf

----------------------------------------
-- Interprocess communication

-- | Lifted 'P.createPipe'.
createPipe :: Process :> es => Eff es (Handle, Handle)
createPipe = unsafeEff_ P.createPipe

-- | Lifted 'P.createPipeFd'.
createPipeFd :: Process :> es => Eff es (FD, FD)
createPipeFd = unsafeEff_ P.createPipeFd
