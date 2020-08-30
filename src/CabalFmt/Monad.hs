-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module CabalFmt.Monad (
    -- * Monad class
    MonadCabalFmt (..),
    getFiles,
    -- * Pure implementation
    CabalFmt,
    runCabalFmt,
    -- * IO implementation
    CabalFmtIO,
    runCabalFmtIO,
    ) where

import Control.Exception      (IOException, catch, throwIO, try)
import Control.Monad          (when)
import Control.Monad.Except   (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader   (MonadReader (..), ReaderT (..), asks, runReaderT)
import Control.Monad.Writer   (WriterT, runWriterT, tell)
import Data.Bifunctor         (first)
import System.Exit            (exitFailure)
import System.FilePath        ((</>))
import System.IO              (hPutStrLn, stderr)

import qualified Data.ByteString  as BS
import qualified Data.Map         as Map
import qualified System.Directory as D

import CabalFmt.Error
import CabalFmt.Options

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | @cabal-fmt@ interface.
--
-- * reader of 'Options'
-- * errors of 'Error'
-- * can list directories
--
class (HasOptions r, MonadReader r m, MonadError Error m) => MonadCabalFmt r m | m -> r where
    listDirectory      :: FilePath -> m [FilePath]
    doesDirectoryExist :: FilePath -> m Bool

    readFileBS         :: FilePath -> m (Maybe BS.ByteString)

    displayWarning     :: String -> m ()

-------------------------------------------------------------------------------
-- Pure
-------------------------------------------------------------------------------

-- | Pure 'MonadCabalFmt'.
--
-- 'listDirectory' always return empty list.
--
newtype CabalFmt a = CabalFmt { unCabalFmt :: ReaderT (Options, Map.Map FilePath BS.ByteString) (WriterT [String] (Either Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Error)

instance MonadReader Options CabalFmt where
    ask = CabalFmt $ asks fst

    local f (CabalFmt m) = CabalFmt $ local (first f) m

instance MonadCabalFmt Options CabalFmt where
    listDirectory _      = return []
    doesDirectoryExist _ = return False
    readFileBS p         = CabalFmt $ do
        files <- asks snd
        return (Map.lookup p files)
    displayWarning w     = do
        werror <- asks optError
        if werror
        then throwError $ WarningError w
        else CabalFmt $ tell [w]

runCabalFmt
    :: Map.Map FilePath BS.ByteString -> Options
    -> CabalFmt a -> Either Error (a, [String])
runCabalFmt files opts m = runWriterT (runReaderT (unCabalFmt m) (opts, files))

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

-- | Options with root for directory traversals
data Options' = Options'
    { optRootDir :: Maybe FilePath
    , optOpt     :: Options
    }

instance HasOptions Options' where
    options f (Options' mfp o) = Options' mfp <$> f o

newtype CabalFmtIO a = CabalFmtIO { unCabalFmtIO :: ReaderT Options' IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Options')

instance MonadError Error CabalFmtIO where
    throwError = liftIO . throwIO
    catchError m h = CabalFmtIO $ ReaderT $ \r ->
        catch (unCabalFmtIO' r m) (unCabalFmtIO' r . h)
      where
        unCabalFmtIO' r m' = runReaderT (unCabalFmtIO m') r

instance MonadCabalFmt Options' CabalFmtIO where
    listDirectory p = do
        rd <- asks optRootDir
        case rd of
            Nothing -> return []
            Just d  -> liftIO (D.listDirectory (d </> p))
    doesDirectoryExist p = do
        rd <- asks optRootDir
        case rd of
            Nothing -> return False
            Just d  -> liftIO (D.doesDirectoryExist (d </> p))
    readFileBS p = do
        rd <- asks optRootDir
        case rd of
            Nothing -> return Nothing
            Just d  -> liftIO $ catchIOError $ BS.readFile (d </> p)
    displayWarning w = do
        werror <- asks (optError . optOpt)
        liftIO $ do
            hPutStrLn stderr $ (if werror then "ERROR: " else "WARNING: ") ++ w
            when werror exitFailure

catchIOError :: IO a -> IO (Maybe a)
catchIOError m = catch (fmap Just m) handler where
    handler :: IOException -> IO (Maybe a)
    handler _ = return Nothing

runCabalFmtIO :: Maybe FilePath -> Options -> CabalFmtIO a -> IO (Either Error a)
runCabalFmtIO mfp opts m = try $ runReaderT (unCabalFmtIO m) (Options' mfp opts)

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

getFiles :: MonadCabalFmt r m => FilePath -> m [FilePath]
getFiles = getDirectoryContentsRecursive' check where
    check "dist-newstyle" = False
    check ('.' : _)       = False
    check _               = True

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
-- /Note:/ From @Cabal@'s "Distribution.Simple.Utils"
getDirectoryContentsRecursive'
    :: forall m r. MonadCabalFmt r m
    => (FilePath -> Bool) -- ^ Check, whether to recurse
    -> FilePath           -- ^ top dir
    -> m [FilePath]
getDirectoryContentsRecursive' ignore' topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> m [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = do
      (files, dirs') <- collect [] [] =<< listDirectory (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore x          = not (ignore' x)
