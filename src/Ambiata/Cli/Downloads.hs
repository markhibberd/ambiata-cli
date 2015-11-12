{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Download/sync files from s3 to client.
--

module Ambiata.Cli.Downloads (
    filesToDownload
  , downloadFiles
  , downloadReady
  , isFileMissing
  ) where

import           Ambiata.Cli.Data

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either

import qualified Data.Text                  as T

import           Mismi
import           Mismi.S3 hiding ((</>))

import           P

import           System.Directory
import           System.FilePath
import           System.IO

-- |
-- Do the download with the given credentials
--
downloadReady :: DownloadDir -> Region -> DownloadAccess -> EitherT AmbiataError IO DownloadResult
downloadReady dir r (DownloadAccess (TemporaryCreds k s sess) a) = do
  env <- setDebugging <$> getDebugging <*> newEnvFromCreds r k s (Just sess)
  bimapEitherT AmbiataAWSDownloadError id . runAWS env $ downloadFiles dir a

-- |
-- Download any files from the remote dir that are not existing locally.
--
downloadFiles :: DownloadDir -> [ServerFile] -> AWS DownloadResult
downloadFiles dir a = do
  toDo <- liftIO $ filesToDownload dir a
  fmap DownloadResult $ mapM (doDownload dir) toDo

doDownload :: DownloadDir -> ServerFile -> AWS LocalFile
doDownload dir a = do
  let fp = serverFilePath a
  liftIO $ putStrLn $ "Downloading " <> fp
  -- NOTE: This will download to a temporary file and then atomically rename once complete
  download (unServerFile a) (unDownloadDir dir </> fp)
  liftIO $ putStrLn $ "Downloading " <> fp <> " [complete]"
  pure . LocalFile . T.pack $ fp

-- |
-- What files should be downloaded
--
filesToDownload :: DownloadDir -> [ServerFile] -> IO [ServerFile]
filesToDownload d a =
  filterM (isFileMissing d) a

isFileMissing :: DownloadDir -> ServerFile -> IO Bool
isFileMissing (DownloadDir d) f = do
  exists' <- doesFileExist $ d </> serverFilePath f
  pure . not $ exists'
