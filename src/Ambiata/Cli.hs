{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ambiata.Cli (
    uploadCommand
  , downloadCommand
  , doUpload
  , doDownload
) where

import           Ambiata.Cli.Api
import           Ambiata.Cli.Data
import           Ambiata.Cli.Downloads
import           Ambiata.Cli.Incoming
import           Ambiata.Cli.Processing
import           Ambiata.Cli.Rest

import qualified Data.Text                    as T
import           Data.Time.Clock

import           Control.Exception
import           Control.Monad.IO.Class       (liftIO)

import           Mismi

import           P

import           System.Exit
import           System.FileLock
import           System.FilePath
import           System.IO
import           System.Log.Logger

import           Twine.Snooze                 (minutes, snooze)

import           X.Control.Monad.Trans.Either

orErrorAndDie :: Text -> (e -> Text) -> EitherT e IO a -> IO a
orErrorAndDie ctx render act =
  runEitherT act >>= either errorAndDie pure
  where errorAndDie err = do
          errorM (T.unpack ctx) (T.unpack $ render err)
          exitWith (ExitFailure 1)

uploadCommand :: UploadEnv -> IO ()
uploadCommand e@(UploadEnv d _ c) = runAction verb "upload" mode (unDir d </> ".ambiata-upload.lock") upload'
 where upload'                  = doUpload e
                                    >>= liftIO . infoM (T.unpack logCtx) . T.unpack . renderUploadResult
       mode                     = runMode c
       verb                     = verbosity c
       logCtx                   = "Ambiata.uploadCommand"

downloadCommand :: DownloadEnv -> IO ()
downloadCommand e@(DownloadEnv d _ _ c) = runAction verb "download" mode (unDownloadDir d </> ".ambiata-download.lock") download'
 where download'                    = doDownload e
                                        >>= liftIO . infoM (T.unpack logCtx) . T.unpack . renderDownloadResult
       mode                         = runMode c
       verb                         = verbosity c
       logCtx                       = "Ambiata.downloadCommand"

runAction :: Verbosity -> Text -> RunMode -> FilePath -> EitherT AmbiataError IO a -> IO a
runAction verb name m lockf act = do
  updateGlobalLogger rootLoggerName (setLevel verbLevel)
  debugM (T.unpack logCtx) $ "Starting " <> (T.unpack name)
  bracket (getLock lockf) unlockFile (go m)
  where go OneShot _ = act'
        go Daemon  _ = forever $ act' >> snooze (minutes 1)

        getLock lp = orErrorAndDie logCtx id $
          eitherTFromMaybe ("Unable to lock " <> T.pack lp <> " - is another process using it?")
            $ tryLockFile lp Exclusive

        verbLevel
          | verb == Verbose = DEBUG
          | otherwise       = INFO

        logCtx = "Ambiata.runAction"

        act' = orErrorAndDie logCtx renderClientError act

doDownload ::  DownloadEnv -> EitherT AmbiataError IO DownloadResult
doDownload (DownloadEnv dir o e c) = do
  creds <- bimapEitherT AmbiataApiError id
    . apiCall (apiKey c) (apiEndpoint c)
    $ obtainCredentialsForDownload o e
  downloadReady dir Sydney creds

doUpload :: UploadEnv -> EitherT AmbiataError IO UploadResult
doUpload (UploadEnv dir retention c) = do
  -- Connect to the API before doing anything else, so we fail fast in
  -- the case of a configuration error.
  creds <- bimapEitherT AmbiataApiError id
    . apiCall (apiKey c) (apiEndpoint c)
    $ obtainCredentialsForUpload
  prepareDir dir
  now   <- liftIO $ getCurrentTime
  (incoming, processing, bads)  <- processDir dir (NoChangeAfter $ twoMinutesBefore now)
  liftIO $ mapM_ warnBadFile bads
  uploaded <- fmap concat $ mapM (const $ uploadReady dir Sydney creds) processing

  liftIO $ debugM logCtx "Cleaning up archived files..."
  cleaned <- cleanUpArchived dir retention now
  liftIO . infoM logCtx $ "Cleaned up archived files: " <> (renderArchived cleaned)
  pure $ UploadResult incoming processing uploaded
  where
    warnBadFile st = warningM logCtx . T.unpack . T.concat $
          [ "Not processing "
          , renderBadFileState st
          ]

    logCtx = "Ambiata.doUpload"

    renderArchived = T.unpack . renderFileList . fmap (unArchivedFile)

twoMinutesBefore :: UTCTime -> UTCTime
twoMinutesBefore now = addUTCTime (-120) now
