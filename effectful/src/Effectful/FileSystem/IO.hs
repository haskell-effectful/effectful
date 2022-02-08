-- | Lifted version of "System.IO".
module Effectful.FileSystem.IO
  ( FileSystem
  , runFileSystem

    -- * IO
  , IOMode (..)
  , Handle
  , IO.stdin
  , IO.stdout
  , IO.stderr
  , withFile
  , withBinaryFile
  , openFile
  , hClose
  , hFlush
  , hFileSize
  , hSetFileSize
  , hIsEOF
  , IO.BufferMode (..)
  , hSetBuffering
  , hGetBuffering
  , hSeek
  , IO.SeekMode (..)
  , hTell
  , hIsOpen
  , hIsClosed
  , hIsReadable
  , hIsWritable
  , hIsSeekable
  , hIsTerminalDevice
  , hSetEcho
  , hGetEcho
  , hWaitForInput
  , hReady
  ) where

import qualified System.IO as IO
import System.IO (Handle, IOMode (..))

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem.Effect

-- | Lifted version of 'IO.withFile'.
withFile
  :: FileSystem :> es
  => FilePath
  -> IOMode
  -> (Handle -> Eff es a)
  -> Eff es a
withFile fp mode inner = unsafeUnliftIO $ \unlift -> do
  IO.withFile fp mode $ unlift . inner

-- | Lifted version of 'IO.withBinaryFile'.
withBinaryFile
  :: FileSystem :> es
  => FilePath
  -> IOMode
  -> (Handle -> Eff es a)
  -> Eff es a
withBinaryFile fp mode inner = unsafeUnliftIO $ \unlift -> do
  IO.withBinaryFile fp mode $ unlift . inner

-- | Lifted version of 'IO.openFile'
openFile :: FileSystem :> es => FilePath -> IOMode -> Eff es Handle
openFile fp = deferIO . IO.openFile fp

-- | Lifted version of 'IO.hClose'
hClose :: FileSystem :> es => Handle -> Eff es ()
hClose = deferIO . IO.hClose

-- | Lifted version of 'IO.hFlush'
hFlush :: FileSystem :> es => Handle -> Eff es ()
hFlush = deferIO . IO.hFlush

-- | Lifted version of 'IO.hFileSize'
hFileSize :: FileSystem :> es => Handle -> Eff es Integer
hFileSize = deferIO . IO.hFileSize

-- | Lifted version of 'IO.hSetFileSize'
hSetFileSize :: FileSystem :> es => Handle -> Integer -> Eff es ()
hSetFileSize h = deferIO . IO.hSetFileSize h

-- | Lifted version of 'IO.hIsEOF'
hIsEOF :: FileSystem :> es => Handle -> Eff es Bool
hIsEOF = deferIO . IO.hIsEOF

-- | Lifted version of 'IO.hSetBuffering'
hSetBuffering :: FileSystem :> es => Handle -> IO.BufferMode -> Eff es ()
hSetBuffering h = deferIO . IO.hSetBuffering h

-- | Lifted version of 'IO.hGetBuffering'
hGetBuffering :: FileSystem :> es => Handle -> Eff es IO.BufferMode
hGetBuffering = deferIO . IO.hGetBuffering

-- | Lifted version of 'IO.hSeek'
hSeek :: FileSystem :> es => Handle -> IO.SeekMode -> Integer -> Eff es ()
hSeek h s = deferIO . IO.hSeek h s

-- | Lifted version of 'IO.hTell'
hTell :: FileSystem :> es => Handle -> Eff es Integer
hTell = deferIO . IO.hTell

-- | Lifted version of 'IO.hIsOpen'
hIsOpen :: FileSystem :> es => Handle -> Eff es Bool
hIsOpen = deferIO . IO.hIsOpen

-- | Lifted version of 'IO.hIsClosed'
hIsClosed :: FileSystem :> es => Handle -> Eff es Bool
hIsClosed = deferIO . IO.hIsClosed

-- | Lifted version of 'IO.hIsReadable'
hIsReadable :: FileSystem :> es => Handle -> Eff es Bool
hIsReadable = deferIO . IO.hIsReadable

-- | Lifted version of 'IO.hIsWritable'
hIsWritable :: FileSystem :> es => Handle -> Eff es Bool
hIsWritable = deferIO . IO.hIsWritable

-- | Lifted version of 'IO.hIsSeekable'
hIsSeekable :: FileSystem :> es => Handle -> Eff es Bool
hIsSeekable = deferIO . IO.hIsSeekable

-- | Lifted version of 'IO.hIsTerminalDevice'
hIsTerminalDevice :: FileSystem :> es => Handle -> Eff es Bool
hIsTerminalDevice = deferIO . IO.hIsTerminalDevice

-- | Lifted version of 'IO.hSetEcho'
hSetEcho :: FileSystem :> es => Handle -> Bool -> Eff es ()
hSetEcho h = deferIO . IO.hSetEcho h

-- | Lifted version of 'IO.hGetEcho'
hGetEcho :: FileSystem :> es => Handle -> Eff es Bool
hGetEcho = deferIO . IO.hGetEcho

-- | Lifted version of 'IO.hWaitForInput'
hWaitForInput :: FileSystem :> es => Handle -> Int -> Eff es Bool
hWaitForInput h = deferIO . IO.hWaitForInput h

-- | Lifted version of 'IO.hReady'
hReady :: FileSystem :> es => Handle -> Eff es Bool
hReady = deferIO . IO.hReady
