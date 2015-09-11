{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TatooineCli (
    uploadCommand
  , downloadCommand
  , doUpload
  , doDownload
) where

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock

import           Control.Exception
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Either

import           Mismi.Control.Amazonka

import           P

import           System.Directory
import           System.Exit
import           System.FileLock
import           System.FilePath
import           System.IO
import           System.Log.Logger

import           TatooineCli.Credentials
import           TatooineCli.Data
import           TatooineCli.Downloads
import           TatooineCli.Incoming
import           TatooineCli.Processing

import           Twine.Snooze                 (minutes, snooze)

import           X.Control.Monad.Trans.Either

orErrorAndDie :: Text -> (e -> Text) -> EitherT e IO a -> IO a
orErrorAndDie ctx render act =
  runEitherT act >>= either errorAndDie pure
  where errorAndDie err = do
          errorM (T.unpack ctx) (T.unpack $ render err)
          exitWith (ExitFailure 1)

uploadCommand :: UploadEnv -> IO ()
uploadCommand e@(UploadEnv _ _ c) = runAction verb "upload" mode "ambiata-upload.lock" upload'
 where upload'                  = doUpload e
                                    >>= liftIO . infoM (T.unpack logCtx) . T.unpack . renderUploadResult
       mode                     = runMode c
       verb                     = verbosity c
       logCtx                   = "TatooineCli.uploadCommand"

downloadCommand :: DownloadEnv -> IO ()
downloadCommand e@(DownloadEnv _ c) = runAction verb "download" mode "ambiata-download.lock" download'
 where download'                    = doDownload e
                                        >>= liftIO . infoM (T.unpack logCtx) . T.unpack . renderDownloadResult
       mode                         = runMode c
       verb                         = verbosity c
       logCtx                       = "TatooineCli.downloadCommand"

-- |
-- We write to the system tmpdir instead of something sane like
-- /var/run because we want to make it as low-friction as possible,
-- and we're more likely to have write permissions in /tmp.
runAction :: Verbosity -> Text -> RunMode -> FilePath -> EitherT TatooineClientError IO a -> IO a
runAction verb name m lockf act = do
  updateGlobalLogger rootLoggerName (setLevel verbLevel)
  debugM (T.unpack logCtx) $ "Starting " <> (T.unpack name)
  tmp <- getTemporaryDirectory
  let lp = tmp </> lockf
  bracket (getLock lp) unlockFile (go m)
  where go OneShot _ = act'
        go Daemon  _ = forever $ act' >> snooze (minutes 1)

        getLock lp = orErrorAndDie logCtx id $
          eitherTFromMaybe ("Unable to lock " <> T.pack lp <> " - is another process using it?")
            $ tryLockFile lp Exclusive

        verbLevel
          | verb == Verbose = DEBUG
          | otherwise       = INFO

        logCtx = "TatooineCli.runAction"

        act' = orErrorAndDie logCtx renderClientError act

doDownload ::  DownloadEnv -> EitherT TatooineClientError IO DownloadResult
doDownload (DownloadEnv dir c) = do
  creds <- bimapEitherT TatooineCredentialLoadError id $ obtainCredentialsForDownload (apiKey c) (apiEndpoint c)
  bimapEitherT TatooineAWSError id $ downloadReady dir Sydney creds

doUpload :: UploadEnv -> EitherT TatooineClientError IO UploadResult
doUpload (UploadEnv dir retention c) = do
  -- Connect to the API before doing anything else, so we fail fast in
  -- the case of a configuration error.
  creds <- bimapEitherT TatooineCredentialLoadError id $ obtainCredentialsForUpload (apiKey c) (apiEndpoint c)
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

    logCtx = "TatooineCli.doUpload"

    renderArchived = T.unpack . renderFileList . fmap (unArchivedFile)

twoMinutesBefore :: UTCTime -> UTCTime
twoMinutesBefore now = addUTCTime (-120) now
