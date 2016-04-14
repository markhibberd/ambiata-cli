{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ambiata.Cli.Standalone (
    UploadError
  , upload
  , upload'
  , uploadExec
  , uploadExec'
  , renderUploadError
  ) where

import           Ambiata.Cli.Api
import           Ambiata.Cli.Data.Api
import           Ambiata.Cli.Data.Exec
import           Ambiata.Cli.Data.Upload
import           Ambiata.Cli.Rest

import           Control.Monad.Catch (bracket_, onException, throwM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Lens ((.~), (^.))

import qualified Data.Conduit as C
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty, nonEmpty)

import           P

import           Mismi (runAWS, runAWST, renderError, configureRetries, getDebugging, setDebugging, newEnvFromCreds)
import           Mismi.Amazonka (Error)
import           Mismi.S3 (Key (..), (//))
import qualified Mismi.S3 as S3
import qualified Mismi.S3.Internal as S3 (f')
import qualified Mismi.S3.Amazonka as A

import           System.IO (IO, FilePath)
import           System.IO.Error (userError)
import           System.FilePath (takeFileName)
import           System.Process (CreateProcess (..))
import qualified System.Process as P
import           System.Exit (ExitCode (..))

import           X.Control.Monad.Trans.Either (EitherT, bimapEitherT, newEitherT, runEitherT, left)

data UploadError =
    UploadApiError ApiError
  | UploadAwsError Error
  | UploadS3Error S3.UploadError
  | UploadNoUploadIdAvailable
  | UploadCantCreatePipe
  | UploadAborted Int

-- |
-- Upload a single file via the API.
--
upload :: A.Region -> AmbiataAPIKey -> AmbiataAPIEndpoint -> FilePath -> EitherT UploadError IO ()
upload region k a f = do
  creds <- bimapEitherT UploadApiError id . apiCall k a $
    obtainCredentialsForUpload
  privileged <- auth region creds
  let target = toTarget creds . FileName . T.pack . takeFileName $ f
  upload' privileged target f

upload' :: A.Env -> S3.Address -> FilePath -> EitherT UploadError IO ()
upload' privileged target f = do
  runAWST privileged UploadAwsError . bimapEitherT UploadS3Error id $
    S3.uploadWithMode S3.Overwrite f target


-- |
-- Execute a program and upload its standard out as the specified filename.
--
-- Note: This is not a great idea, it will be _slow_ and potentially
-- far less reliable than uploading a file directly, but may be
-- convenient for small data.
--
uploadExec :: A.Region -> AmbiataAPIKey -> AmbiataAPIEndpoint -> FileName -> Program -> Arguments -> BufferSize -> EitherT UploadError IO ()
uploadExec region k a n p args b = do
  creds <- bimapEitherT UploadApiError id . apiCall k a $ obtainCredentialsForUpload
  privileged <- auth region creds
  uploadExec' privileged (toTarget creds n) p args b

uploadExec' :: A.Env -> S3.Address -> Program -> Arguments -> BufferSize -> EitherT UploadError IO ()
uploadExec' privileged target p args b = do
  let
    create =
      S3.f' A.createMultipartUpload target
        & A.cmuServerSideEncryption .~ Just S3.sse

    part z uploadId chunk chunkSize =
      S3.f' A.uploadPart target z uploadId
        $ A.Chunked $ A.ChunkedBody (A.ChunkSize chunkSize) (fromIntegral chunkSize) (C.yield chunk)

    abort uploadId =
      S3.f' A.abortMultipartUpload target uploadId

    finalise uploadId parts =
      S3.f' A.completeMultipartUpload target uploadId
        & A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ (convert . reverse $ parts))

    withPrivilege =
      bimapEitherT UploadAwsError id . runAWS privileged

    withPrivilegeT =
      runAWST privileged UploadAwsError

  mpu <- withPrivilegeT . A.send $ create

  uploadId <- fromMaybeM (left UploadNoUploadIdAvailable) $ mpu ^. A.cmursUploadId

  (_, outx, _, handle) <- liftIO . P.createProcess $
    (procify p args) { std_out = P.CreatePipe, std_err = P.Inherit, std_in = P.Inherit }

  out <- fromMaybeM (left UploadCantCreatePipe) $ outx

  parts <- newEitherT . bracket_ (pure ()) (P.terminateProcess handle) . runEitherT $ do
    let byChunk !z !cs = do
          chunk <- liftIO $ BS.hGet out (bufferSize b)
          case BS.length chunk of
            0 ->
              pure cs
            x -> do
              r <- withPrivilege . A.send $ part z uploadId chunk x
              m <- fromMaybeM (throwM . userError $ "invariant: unset etag on upload part. ") $ r ^. A.uprsETag
              byChunk (z + 1) (m : cs)
    byChunk 1 []

  r <- liftIO $ P.waitForProcess handle

  case r of
    ExitFailure x -> do
      void . withPrivilege . A.send $ abort uploadId
      left $ UploadAborted x

    ExitSuccess ->
      flip onException (withPrivilege . A.send $ abort uploadId) $ do
        when (not . null $ parts) $
          void . withPrivilege . A.send $ finalise uploadId parts
        when (null $ parts) $ do
          void . withPrivilege . A.send $ abort uploadId
          void . withPrivilege $ S3.writeWithModeOrFail S3.Overwrite target ""

toTarget :: UploadAccess -> FileName -> S3.Address
toTarget creds n =
  S3.withKey (// Key (fileName n)) . s3Path . unUploadAccess $ creds

auth :: A.Region -> UploadAccess -> EitherT e IO A.Env
auth region creds =
  fmap (configureRetries 10) . setDebugging
    <$> getDebugging
    <*> newEnvFromCreds
      region
      (tempKey . tempCreds . unUploadAccess $ creds)
      (tempSecret . tempCreds . unUploadAccess  $ creds)
      (Just . sessionToken . tempCreds . unUploadAccess $ creds)

convert :: [A.ETag] -> Maybe (NonEmpty A.CompletedPart)
convert parts =
 nonEmpty . flip fmap (L.zip parts [1..]) $ \(p, i) ->
   A.completedPart i p

renderUploadError :: UploadError -> Text
renderUploadError err =
  case err of
    UploadApiError e ->
      mconcat ["There was an error contacting the Ambiata API - ", renderApiError e]
    UploadAwsError e ->
      mconcat ["There was an error contacting the Amazon API - ", renderError e]
    UploadS3Error e ->
      mconcat ["There was an error uploading to S3 - ", S3.renderUploadError e]
    UploadNoUploadIdAvailable ->
      mconcat ["Amazon didn't return a multipart id, can't continue."]
    UploadCantCreatePipe ->
      mconcat ["Couldn't create a pipe to your executable."]
    UploadAborted e ->
      mconcat ["Exec process failed with status - ", T.pack . show $ e]
