{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This deals with files that are ready to ingest.
--

module TatooineCli.Processing (
    availableFiles,
    moveToArchive,
    uploadAction,
    processReady,
    fileAddress,
    uploadReady
) where

import           P

import           System.Directory
import           System.FilePath              hiding ((</>))
import           System.IO

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Either

import qualified Data.Text                    as T

import           TatooineCli.Data
import           TatooineCli.Json

import           Mismi.Control.Amazonka
import           Mismi.S3.Commands
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           X.Control.Monad.Trans.Either

-- |
-- Upload the files that are in the processing dir, and move to archive when done.
--
uploadReady :: IncomingDir -> Region -> UploadAccess -> EitherT TatooineClientError IO [ArchivedFile]
uploadReady dir r (UploadAccess (TemporaryAccess (TemporaryCreds k s sess) a)) =
  firstEitherT TatooineAWSError $ runAWSWithCreds r k s (Just $ sess) Nothing $ processReady dir a


processReady :: IncomingDir -> Address -> AWS [ArchivedFile]
processReady dir a = do
  files <- liftIO $ availableFiles dir
  mapM (uploadAction dir a) files


uploadAction :: IncomingDir -> Address -> ProcessingFile -> AWS ArchivedFile
uploadAction dir a f@(ProcessingFile fname) = do
  retryAWSAction (retryWithBackoff 10) $ uploadWithMode Overwrite fpath (fileAddress f a)
  liftIO $ moveToArchive dir f
  where
    fpath = toWorkingPath dir Processing <//> T.unpack fname

fileAddress :: ProcessingFile -> Address -> Address
fileAddress (ProcessingFile fname) (Address b k) =
    Address b (k </> Key fname)

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
