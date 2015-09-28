{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Download/sync files from s3 to client.
--

module Ambiata.Cli.Downloads (
    serverFiles
  , filesToDownload
  , downloadFiles
  , downloadReady
  , isFileMissing
) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Json

import           P

import           System.Directory
import           System.FilePath
import           System.IO

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either

import qualified Data.Text                  as T

import           Mismi.S3.Default
import           Mismi.S3.Data              hiding ((</>))

import           Mismi.Control.Amazonka

-- |
-- Do the download with the given credentials
--
downloadReady :: DownloadDir -> Region -> DownloadAccess -> EitherT AWSError IO DownloadResult
downloadReady dir r (DownloadAccess (TemporaryAccess (TemporaryCreds k s sess) a)) =
  runAWSWithCreds r k s (Just $ sess) $ downloadFiles dir a

-- |
-- Download any files from the remote dir that are not existing locally.
--
downloadFiles :: DownloadDir -> Address -> AWS DownloadResult
downloadFiles dir a = do
  toDo <- filesToDownload dir withTrailingSlash
  fmap DownloadResult $ mapM (doDownload dir a) toDo
  where
    -- NOTE: required due to: https://github.com/ambiata/mismi/issues/158
    -- can remove this when issue is resolved. combineKey ensures trailing for now.
    withTrailingSlash = withKey (`combineKey` Key "") a

doDownload :: DownloadDir -> Address -> ServerFile -> AWS LocalFile
doDownload dir a (ServerFile name) = do
  download (withKey (`combineKey` Key name) a) localFile
  pure . LocalFile $ name
  where
    localFile = (unDownloadDir dir) </> T.unpack name

-- |
-- What files should be downloaded
--
filesToDownload :: DownloadDir -> Address -> AWS [ServerFile]
filesToDownload d a = do
  ls <- serverFiles a
  filterM (isFileMissing d) ls

isFileMissing :: DownloadDir -> ServerFile -> AWS Bool
isFileMissing (DownloadDir d) (ServerFile f) = do
  exists' <- liftIO . doesFileExist $ d </> T.unpack f
  pure . not $ exists'

-- |
-- See what is up on the server we may want to pull down
--
serverFiles :: Address -> AWS [ServerFile]
serverFiles a = do
  ls <- listRecursively a
  pure $ ServerFile . unKey <$> catMaybes (removeCommonPrefix a <$> ls)
