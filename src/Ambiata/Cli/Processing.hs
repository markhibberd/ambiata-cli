{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This deals with files that are ready to ingest.
--

module Ambiata.Cli.Processing (
    availableFiles
  , moveToArchive
  , uploadAction
  , processReady
  , fileAddress
  , uploadReady
  ) where

import           Ambiata.Cli.Data

import           Control.Lens (over, set)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (local)

import qualified Data.Text                    as T

import           Mismi
import           Mismi.Amazonka (serviceRetry, retryAttempts, exponentBase, configure)
import           Mismi.S3
import           Mismi.S3.Amazonka (s3)

import           P

import           System.Directory
import           System.FilePath              hiding ((</>))
import           System.IO

import           X.Control.Monad.Trans.Either


-- |
-- Upload the files that are in the processing dir, and move to archive when done.
--
uploadReady :: IncomingDir -> Region -> UploadAccess -> EitherT AmbiataError IO [ArchivedFile]
uploadReady dir r (UploadAccess (TemporaryAccess (TemporaryCreds k s sess) a)) = do
  env' <- setDebugging <$> getDebugging <*> newEnvFromCreds r k s (Just sess)
  let
    env = configure (over serviceRetry (set retryAttempts 10 . set exponentBase 0.6) s3) env'
  bimapEitherT AmbiataAWSUploadError id $ runAWS env
    . local (configureRetries 10)
    $ processReady dir a


processReady :: IncomingDir -> Address -> AWS [ArchivedFile]
processReady dir a = do
  files <- liftIO $ availableFiles dir
  mapM (uploadAction dir a) files


uploadAction :: IncomingDir -> Address -> ProcessingFile -> AWS ArchivedFile
uploadAction dir a f@(ProcessingFile fname) = do
  uploadWithModeOrFail Overwrite fpath (fileAddress f a)
  liftIO $ moveToArchive dir f
  where
    fpath = toWorkingPath dir Processing <//> T.unpack fname

fileAddress :: ProcessingFile -> Address -> Address
fileAddress (ProcessingFile fname) (Address b k) =
    Address b (k // Key fname)

-- |
-- Stuff ready to go
--
availableFiles :: IncomingDir -> IO [ProcessingFile]
availableFiles dir = do
  fileList <- (getDirectoryContents $ d) >>= filterM (doesFileExist . (<//>) d)
  pure $ (ProcessingFile . T.pack) <$> fileList
  where
    d = toWorkingPath dir Processing

moveToArchive :: IncomingDir -> ProcessingFile -> IO ArchivedFile
moveToArchive dir (ProcessingFile fname) = do
  renameFile (toWorkingPath dir Processing <//> T.unpack fname)
             (toWorkingPath dir Archive <//> T.unpack fname)
  pure $ ArchivedFile fname



(<//>) :: FilePath -> FilePath -> FilePath
(<//>) = combine
