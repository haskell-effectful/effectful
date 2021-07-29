{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module FileSizes where

import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import System.Posix

-- effectful
import qualified Effectful as E
import qualified Effectful.Reader as E
import qualified Effectful.State.Local as E

-- eff
#ifdef VERSION_eff
import qualified Control.Effect as L
#endif

-- freer-simple
#ifdef VERSION_freer_simple
import qualified Control.Monad.Freer as FS
import qualified Control.Monad.Freer.Reader as FS
import qualified Control.Monad.Freer.State as FS
#endif

-- mtl
#ifdef VERSION_mtl
import qualified Control.Monad.State as M
import qualified Control.Monad.Reader as M
#endif

-- polysemy
#ifdef VERSION_polysemy
import qualified Polysemy as P
import qualified Polysemy.Reader as P
import qualified Polysemy.State as P
import qualified Polysemy.Internal as P
#endif

tryGetFileSize :: FilePath -> IO (Maybe Int)
tryGetFileSize path = try @IOException (getFileStatus path) >>= \case
  Left  _    -> pure Nothing
  Right stat -> pure . Just . fromIntegral $ fileSize stat

----------------------------------------
-- reference

ref_calculateFileSize :: IORef [String] -> FilePath -> IO Int
ref_calculateFileSize logs path = do
  logToIORef logs $ "Calculating the size of " ++ path
  tryGetFileSize path >>= \case
    Nothing -> 0      <$ logToIORef logs ("Could not calculate the size of " ++ path)
    Just size -> size <$ logToIORef logs (path ++ " is " ++ show size ++ " bytes")
  where
    logToIORef :: IORef [String] -> String -> IO ()
    logToIORef r msg = modifyIORef' r (msg :)
{-# NOINLINE ref_calculateFileSize #-}

ref_program :: IORef [String] -> [FilePath] -> IO Int
ref_program logs files = do
  sizes <- traverse (ref_calculateFileSize logs) files
  pure $ sum sizes
{-# NOINLINE ref_program #-}

ref_calculateFileSizes :: [FilePath] -> IO (Int, [String])
ref_calculateFileSizes files = do
  logs      <- newIORef []
  size      <- ref_program logs files
  finalLogs <- readIORef logs
  pure (size, finalLogs)

----------------------------------------
-- effectful

data Effectful_File :: E.Effect where
  Effectful_tryFileSize :: FilePath -> Effectful_File m (Maybe Int)

effectful_tryFileSize :: Effectful_File E.:> es => FilePath -> E.Eff es (Maybe Int)
effectful_tryFileSize = E.send . Effectful_tryFileSize

effectful_runFile :: E.IOE E.:> es => E.Eff (Effectful_File : es) a -> E.Eff es a
effectful_runFile = E.interpret \_ -> \case
  Effectful_tryFileSize path -> liftIO $ tryGetFileSize path

data Effectful_Logging :: E.Effect where
  Effectful_logMsg :: String -> Effectful_Logging m ()

effectful_logMsg :: Effectful_Logging E.:> es => String -> E.Eff es ()
effectful_logMsg = E.send . Effectful_logMsg

effectful_runLogging
  :: E.Eff (Effectful_Logging : es) a
  -> E.Eff es (a, [String])
effectful_runLogging = E.reinterpret (E.runStateE []) \_ -> \case
  Effectful_logMsg msg -> E.modify (msg :)

----------

effectful_calculateFileSize
  :: (Effectful_File E.:> es, Effectful_Logging E.:> es)
  => FilePath
  -> E.Eff es Int
effectful_calculateFileSize path = do
  effectful_logMsg $ "Calculating the size of " ++ path
  effectful_tryFileSize path >>= \case
    Nothing   -> 0    <$ effectful_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ effectful_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE effectful_calculateFileSize #-}

effectful_program
  :: (Effectful_File E.:> es, Effectful_Logging E.:> es)
  => [FilePath]
  -> E.Eff es Int
effectful_program files = do
  sizes <- traverse effectful_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE effectful_program #-}

effectful_calculateFileSizes :: [FilePath] -> IO (Int, [String])
effectful_calculateFileSizes =
  E.runEff . effectful_runFile . effectful_runLogging . effectful_program

effectful_calculateFileSizesDeep :: [FilePath] -> IO (Int, [String])
effectful_calculateFileSizesDeep = E.runEff
  . runR . runR . runR . runR . runR
  . effectful_runFile . effectful_runLogging
  . runR . runR . runR . runR . runR
  . effectful_program
  where
    runR = E.runReaderE ()

----------------------------------------
-- eff

#ifdef VERSION_eff

data Eff_File :: L.Effect where
  Eff_tryFileSize :: FilePath -> Eff_File m (Maybe Int)

eff_tryFileSize :: Eff_File L.:< es => FilePath -> L.Eff es (Maybe Int)
eff_tryFileSize = L.send . Eff_tryFileSize

eff_runFile :: L.IOE L.:< es => L.Eff (Eff_File : es) a -> L.Eff es a
eff_runFile = L.interpret \case
  Eff_tryFileSize path -> liftIO $ tryGetFileSize path

data Eff_Logging :: L.Effect where
  Eff_logMsg :: String -> Eff_Logging m ()

eff_logMsg :: Eff_Logging L.:< es => String -> L.Eff es ()
eff_logMsg = L.send . Eff_logMsg

eff_runLogging
  :: L.Eff (Eff_Logging : es) a
  -> L.Eff es (a, [String])
eff_runLogging
  = fmap eff_swap . L.runState [] . L.interpret \case
      Eff_logMsg msg -> L.modify (msg :)
  . L.lift

eff_swap :: (a, b) -> (b, a)
eff_swap (x, y) = (y, x)

----------

eff_calculateFileSize
  :: (Eff_File L.:< es, Eff_Logging L.:< es)
  => FilePath
  -> L.Eff es Int
eff_calculateFileSize path = do
  eff_logMsg $ "Calculating the size of " ++ path
  eff_tryFileSize path >>= \case
    Nothing   -> 0    <$ eff_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ eff_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE eff_calculateFileSize #-}

eff_program
  :: (Eff_File L.:< es, Eff_Logging L.:< es)
  => [FilePath]
  -> L.Eff es Int
eff_program files = do
  sizes <- traverse eff_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE eff_program #-}

eff_calculateFileSizes :: [FilePath] -> IO (Int, [String])
eff_calculateFileSizes =
  L.runIO . eff_runFile . eff_runLogging . eff_program

eff_calculateFileSizesDeep :: [FilePath] -> IO (Int, [String])
eff_calculateFileSizesDeep = L.runIO
  . runR . runR . runR . runR . runR
  . eff_runFile . eff_runLogging
  . runR . runR . runR . runR . runR
  . eff_program
  where
    runR = L.runReader ()

#endif

----------------------------------------
-- freer-simple

#ifdef VERSION_freer_simple

data FS_File r where
  FS_tryFileSize :: FilePath -> FS_File (Maybe Int)

fs_tryFileSize :: FS.Member FS_File es => FilePath -> FS.Eff es (Maybe Int)
fs_tryFileSize = FS.send . FS_tryFileSize

fs_runFile :: FS.LastMember IO es => FS.Eff (FS_File : es) a -> FS.Eff es a
fs_runFile = FS.interpret \case
  FS_tryFileSize path -> liftIO $ tryGetFileSize path

data FS_Logging r where
  FS_logMsg :: String -> FS_Logging ()

fs_logMsg :: FS.Member FS_Logging es => String -> FS.Eff es ()
fs_logMsg = FS.send . FS_logMsg

fs_runLogging
  :: FS.Eff (FS_Logging : es) a
  -> FS.Eff es (a, [String])
fs_runLogging = FS.runState [] . FS.reinterpret \case
  FS_logMsg msg -> FS.modify (msg :)

----------

fs_calculateFileSize
  :: (FS.Member FS_File es, FS.Member FS_Logging es)
  => FilePath
  -> FS.Eff es Int
fs_calculateFileSize path = do
  fs_logMsg $ "Calculating the size of " ++ path
  fs_tryFileSize path >>= \case
    Nothing   -> 0    <$ fs_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ fs_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE fs_calculateFileSize #-}

fs_program
  :: (FS.Member FS_File es, FS.Member FS_Logging es)
  => [FilePath]
  -> FS.Eff es Int
fs_program files = do
  sizes <- traverse fs_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE fs_program #-}

fs_calculateFileSizes :: [FilePath] -> IO (Int, [String])
fs_calculateFileSizes =
  FS.runM . fs_runFile . fs_runLogging . fs_program

fs_calculateFileSizesDeep :: [FilePath] -> IO (Int, [String])
fs_calculateFileSizesDeep = FS.runM
  . runR . runR . runR . runR . runR
  . fs_runFile . fs_runLogging
  . runR . runR . runR . runR . runR
  . fs_program
  where
    runR = FS.runReader ()

#endif

----------------------------------------
-- mtl

#ifdef VERSION_mtl

class Monad m => MonadFile m where
  mtl_tryFileSize :: FilePath -> m (Maybe Int)

newtype FileT m a = FileT { runFileT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance M.MonadTrans FileT where
  lift = FileT

instance {-# OVERLAPPABLE #-}
  ( MonadFile m
  , Monad (t m)
  , M.MonadTrans t
  ) => MonadFile (t m) where
  mtl_tryFileSize = M.lift . mtl_tryFileSize

instance MonadIO m => MonadFile (FileT m) where
  mtl_tryFileSize path = liftIO $ tryGetFileSize path

class Monad m => MonadLog m where
  mtl_logMsg :: String -> m ()

newtype LoggingT m a = LoggingT (M.StateT [String] m a)
  deriving (Functor, Applicative, Monad, MonadIO, M.MonadTrans)

instance {-# OVERLAPPABLE #-}
  ( MonadLog m
  , Monad (t m)
  , M.MonadTrans t
  ) => MonadLog (t m) where
  mtl_logMsg = M.lift . mtl_logMsg

instance MonadIO m => MonadLog (LoggingT m) where
  mtl_logMsg msg = LoggingT $ M.modify (msg :)

runLoggingT :: LoggingT m a -> m (a, [String])
runLoggingT (LoggingT m) = M.runStateT m []

----------

mtl_calculateFileSize :: (MonadLog m, MonadFile m) => FilePath -> m Int
mtl_calculateFileSize path = do
  mtl_logMsg $ "Calculating the size of " ++ path
  mtl_tryFileSize path >>= \case
    Nothing   -> 0    <$ mtl_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ mtl_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE mtl_calculateFileSize #-}

mtl_program :: (MonadLog m, MonadFile m) => [FilePath] -> m Int
mtl_program files = do
  sizes <- traverse mtl_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE mtl_program #-}

mtl_calculateFileSizes :: [FilePath] -> IO (Int, [String])
mtl_calculateFileSizes = runFileT . runLoggingT . mtl_program

mtl_calculateFileSizesDeep :: [FilePath] -> IO (Int, [String])
mtl_calculateFileSizesDeep
  = runR . runR . runR . runR . runR
  . runFileT . runLoggingT
  . runR . runR . runR . runR . runR
  . mtl_program
  where
    runR = flip M.runReaderT ()

#endif

----------------------------------------
-- polysemy

#ifdef VERSION_polysemy

data Poly_File :: E.Effect where
  Poly_tryFileSize :: FilePath -> Poly_File m (Maybe Int)

poly_tryFileSize :: P.Member Poly_File es => FilePath -> P.Sem es (Maybe Int)
poly_tryFileSize = P.send . Poly_tryFileSize

poly_runFile :: P.Member (P.Embed IO) es => P.Sem (Poly_File : es) a -> P.Sem es a
poly_runFile = P.interpret \case
  Poly_tryFileSize path -> P.embed $ tryGetFileSize path

data Poly_Logging :: E.Effect where
  Poly_logMsg :: String -> Poly_Logging m ()

poly_logMsg :: P.Member Poly_Logging es => String -> P.Sem es ()
poly_logMsg = P.send . Poly_logMsg

poly_runLogging :: P.Sem (Poly_Logging : es) a -> P.Sem es (a, [String])
poly_runLogging = fmap poly_swap . P.runState [] . P.reinterpret \case
  Poly_logMsg msg -> P.modify (msg :)

poly_swap :: (a, b) -> (b, a)
poly_swap (x, y) = (y, x)

----------

poly_calculateFileSize
  :: (P.Member Poly_File es, P.Member Poly_Logging es)
  => FilePath
  -> P.Sem es Int
poly_calculateFileSize path = do
  poly_logMsg $ "Calculating the size of " ++ path
  poly_tryFileSize path >>= \case
    Nothing   -> 0    <$ poly_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ poly_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE poly_calculateFileSize #-}

poly_program
  :: (P.Member Poly_File es, P.Member Poly_Logging es)
  => [FilePath]
  -> P.Sem es Int
poly_program files = do
  sizes <- traverse poly_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE poly_program #-}

poly_calculateFileSizes :: [FilePath] -> IO (Int, [String])
poly_calculateFileSizes =
  P.runM . poly_runFile . poly_runLogging . poly_program

poly_calculateFileSizesDeep :: [FilePath] -> IO (Int, [String])
poly_calculateFileSizesDeep = P.runM
  . runR . runR . runR . runR . runR
  . poly_runFile . poly_runLogging
  . runR . runR . runR . runR . runR
  . poly_program
  where
    runR = P.runReader ()
#endif
