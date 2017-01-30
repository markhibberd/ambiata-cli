{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Download/sync files from s3 to client.
--

module Ambiata.Cli.Downloads (
    DownloadError (..)
  , filesToDownload
  , downloadFiles
  , downloadReady
  , removeOldMarkers
  , isFileMissing
  , markerDir
  , markerFile
  ) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Files

import           Control.Lens (over, set)
import           Control.Monad.Catch (handle, throwM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import qualified Data.Text                  as T

import           Mismi
import           Mismi.Amazonka (serviceRetry, retryAttempts, exponentBase, configure)
import           Mismi.S3 hiding (DownloadError)
import           Mismi.S3.Amazonka (s3)

import           P

import           System.Directory
import           System.FilePath
import           System.IO

import           X.Control.Monad.Trans.Either

{-
The layout of the DOWNLOAD_DIR looks like the following:

  DOWNLOAD_DIR/
    .marker/
      file1
      file2
    file2

Marker files are created when files are downloaded, to ensure they don't
need to be downloaded again.

This allows for users to manage the list of downloaded files separately without
impacting the behaviour of this program.
-}


-- |
-- Do the download with the given credentials
--
downloadReady :: DownloadDir -> Region -> DownloadAccess -> EitherT AmbiataError IO DownloadResult
downloadReady dir r (DownloadAccess (TemporaryCreds k s sess) a) = do
  env' <- setDebugging <$> getDebugging <*> newEnvFromCreds r k s (Just sess)
  let
    env = configure (over serviceRetry (set retryAttempts 10 . set exponentBase 0.6) s3) env'
  runAWST env AmbiataAWSDownloadError
    . bimapEitherT AmbiataDownloadError id . EitherT
    $ downloadFiles dir a

-- |
-- Download any files from the remote dir that are not existing locally.
--
downloadFiles :: DownloadDir -> [ServerFile] -> AWS (Either DownloadError DownloadResult)
downloadFiles dir a = do
  liftIO $ createDirectoryIfMissing True (markerDir dir)
  void . liftIO $ removeOldMarkers dir a
  toDo <- liftIO $ filesToDownload dir a
  runEitherT . fmap DownloadResult $ mapM (EitherT . doDownload dir) toDo

doDownload :: DownloadDir -> ServerFile -> AWS (Either DownloadError LocalFile)
doDownload dir a = runEitherT $ do
  let fp = serverFilePath a
  liftIO $ putStrLn $ "Downloading " <> fp
  -- NOTE: This will download to a temporary file and then atomically rename once complete
  handle (\e -> case e of
      SourceMissing _ _ -> left $ ServerFileDoesNotExist a
      e' -> throwM e'
    )
    . lift $ downloadWithModeOrFail Overwrite (unServerFile a) (unDownloadDir dir </> fp)
  -- Only after the file is finished create a marker
  liftIO $ createMarkerFile dir a
  liftIO $ putStrLn $ "Downloading " <> fp <> " [complete]"
  pure . LocalFile . T.pack $ fp

-- |
-- What files should be downloaded
--
filesToDownload :: DownloadDir -> [ServerFile] -> IO [ServerFile]
filesToDownload d =
  filterM (isFileMissing d)

isFileMissing :: DownloadDir -> ServerFile -> IO Bool
isFileMissing d f =
  fmap not . doesFileExist $ markerFile d f

-- |
-- Remove any markers no longer contained in the list of server files.
-- NOTE: This assumes that the list of files is complete and not paged.
--
removeOldMarkers :: DownloadDir -> [ServerFile] -> IO [LocalFile]
removeOldMarkers d a = do
  let names = fmap serverFilePath a
  let ignoreSystem = not . flip elem [".", ".."]
  getDirectoryContents (markerDir d) >>= \x -> fmap concat . for (filter ignoreSystem x) $ \f -> do
    if not $ elem (takeFileName f) names
      then
        ignoreNoFile (removeFile ((markerDir d) </> f)) >> (pure . pure . LocalFile . T.pack) f
      else pure []


markerDir :: DownloadDir -> FilePath
markerDir (DownloadDir d) =
  d </> ".marker"

markerFile :: DownloadDir -> ServerFile -> FilePath
markerFile d f =
  markerDir d </> serverFilePath f

createMarkerFile :: DownloadDir -> ServerFile -> IO ()
createMarkerFile dir a =
  writeFile (markerFile dir a) ""
