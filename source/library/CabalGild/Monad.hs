{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalGild.Monad where

import CabalGild.Error
import CabalGild.Options
import Control.Exception
  ( IOException,
    catch,
    displayException,
    throwIO,
    try,
  )
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified System.Directory as D
import System.Exit (exitFailure)
import System.FilePath (pathSeparator, (</>))
import System.IO (hPutStrLn, stderr)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | @cabal-gild@ interface.
--
-- * reader of 'Options'
-- * errors of 'Error'
-- * can list directories
class (HasOptions r, MonadReader r m, MonadError Error m) => MonadCabalGild r m | m -> r where
  listDirectory :: FilePath -> m [FilePath]
  doesDirectoryExist :: FilePath -> m Bool

  readFileBS :: FilePath -> m Contents

  displayWarning :: String -> m ()

data Contents
  = Contents BS.ByteString
  | NoIO
  | IOError String

-------------------------------------------------------------------------------
-- Pure
-------------------------------------------------------------------------------

-- | Pure 'MonadCabalGild'.
--
-- 'listDirectory' always return empty list.
newtype CabalGild a = CabalGild {unCabalGild :: ReaderT (Options, Map.Map FilePath BS.ByteString) (WriterT [String] (Either Error)) a}
  deriving newtype (Functor, Applicative, Monad, MonadError Error)

instance MonadReader Options CabalGild where
  ask = CabalGild $ asks fst

  local f (CabalGild m) = CabalGild $ local (first f) m

instance MonadCabalGild Options CabalGild where
  listDirectory dir = CabalGild $ do
    files <- asks snd
    return $ mapMaybe f (Map.keys files)
    where
      f :: FilePath -> Maybe FilePath
      f fp = do
        rest <- stripPrefix (dir ++ [pathSeparator]) fp
        return $ takeWhile (/= pathSeparator) rest

  doesDirectoryExist dir = CabalGild $ do
    files <- asks snd
    return (any (isPrefixOf (dir ++ [pathSeparator])) (Map.keys files))

  readFileBS p = CabalGild $ do
    files <- asks snd
    return (maybe (IOError "doesn't exist") Contents $ Map.lookup p files)

  displayWarning w = do
    werror <- asks optError
    if werror
      then throwError $ WarningError w
      else CabalGild $ tell [w]

runCabalGild ::
  Map.Map FilePath BS.ByteString ->
  Options ->
  CabalGild a ->
  Either Error (a, [String])
runCabalGild files opts m = runWriterT (runReaderT (unCabalGild m) (opts, files))

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

-- | Options with root for directory traversals
data Options' = Options'
  { optRootDir :: Maybe FilePath,
    optOpt :: Options
  }

instance HasOptions Options' where
  options f (Options' mfp o) = Options' mfp <$> f o

newtype CabalGildIO a = CabalGildIO {unCabalGildIO :: ReaderT Options' IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Options')

instance MonadError Error CabalGildIO where
  throwError = liftIO . throwIO
  catchError m h = CabalGildIO $ ReaderT $ \r ->
    catch (unCabalGildIO' r m) (unCabalGildIO' r . h)
    where
      unCabalGildIO' :: Options' -> CabalGildIO a -> IO a
      unCabalGildIO' r m' = runReaderT (unCabalGildIO m') r

instance MonadCabalGild Options' CabalGildIO where
  listDirectory p = do
    rd <- asks optRootDir
    case rd of
      Nothing -> return []
      Just d -> liftIO (D.listDirectory (d </> p))
  doesDirectoryExist p = do
    rd <- asks optRootDir
    case rd of
      Nothing -> return False
      Just d -> liftIO (D.doesDirectoryExist (d </> p))
  readFileBS p = do
    rd <- asks optRootDir
    case rd of
      Nothing -> return NoIO
      Just d -> liftIO $ catchIOError $ BS.readFile (d </> p)
  displayWarning w = do
    werror <- asks (optError . optOpt)
    liftIO $ do
      hPutStrLn stderr $ (if werror then "ERROR: " else "WARNING: ") ++ w
      when werror exitFailure

catchIOError :: IO BS.ByteString -> IO Contents
catchIOError m = catch (fmap Contents m) handler
  where
    handler :: IOException -> IO Contents
    handler exc = return (IOError (displayException exc))

runCabalGildIO :: Maybe FilePath -> Options -> CabalGildIO a -> IO (Either Error a)
runCabalGildIO mfp opts m = try $ runReaderT (unCabalGildIO m) (Options' mfp opts)

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

getFiles :: (MonadCabalGild r m) => FilePath -> m [FilePath]
getFiles = getDirectoryContentsRecursive' check
  where
    check "dist-newstyle" = False
    check ('.' : _) = False
    check _ = True

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
-- /Note:/ From @Cabal@'s "Distribution.Simple.Utils"
getDirectoryContentsRecursive' ::
  forall m r.
  (MonadCabalGild r m) =>
  -- | Check, whether to recurse
  (FilePath -> Bool) ->
  -- | top dir
  FilePath ->
  m [FilePath]
getDirectoryContentsRecursive' ignore' topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> m [FilePath]
    recurseDirectories [] = return []
    recurseDirectories (dir : dirs) = do
      (files, dirs') <- collect [] [] =<< listDirectory (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')
      where
        collect :: [FilePath] -> [FilePath] -> [[Char]] -> m ([FilePath], [FilePath])
        collect files dirs' [] =
          return
            ( reverse files,
              reverse dirs'
            )
        collect files dirs' (entry : entries)
          | ignore entry =
              collect files dirs' entries
        collect files dirs' (entry : entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry : dirs') entries
            else collect (dirEntry : files) dirs' entries

        ignore ['.'] = True
        ignore ['.', '.'] = True
        ignore x = not (ignore' x)
