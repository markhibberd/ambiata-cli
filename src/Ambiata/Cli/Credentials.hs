{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This deals with files that are ready to ingest.
--

module Ambiata.Cli.Credentials (
    obtainCredentialsForUpload
  , obtainCredentialsForDownload
  , renderCredentialError
  , combinePathSafe
) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Http
import           Ambiata.Cli.Json

import           P

import           System.IO

import qualified Data.ByteString.Char8      as BS8
import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding

import           Control.Monad.Catch        (catch)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either

import           Data.Aeson

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types


obtainCredentialsForUpload :: AmbiataAPIKey -> AmbiataAPIEndpoint -> EitherT CredentialLoadError IO UploadAccess
obtainCredentialsForUpload k e =
  fmap UploadAccess $ obtainCredentials Upload k e

obtainCredentialsForDownload :: AmbiataAPIKey -> AmbiataAPIEndpoint -> EitherT CredentialLoadError IO DownloadAccess
obtainCredentialsForDownload k e =
  fmap DownloadAccess $ obtainCredentials Download k e

obtainCredentials :: PermissionDirection -> AmbiataAPIKey -> AmbiataAPIEndpoint -> EitherT CredentialLoadError IO TemporaryAccess
obtainCredentials pd key' (AmbiataAPIEndpoint ep)  = do
  m <- liftIO $ newManager tlsManagerSettings
  req <- parseUrl $ T.unpack ep
  (res, _) <- (liftIO . httpGo httpRetryPolicy m $ getTokenRequest pd req key') `catch` (\(e :: HttpException) -> left $ NetworkException e)
  case responseStatus res of
    (Status 200 _) ->
      hoistEither . first (DecodeError . pack) . eitherDecode $ responseBody res
    (Status _ err) ->
      left . BadResponse . decodeUtf8 $ err

getTokenRequest :: PermissionDirection -> Request -> AmbiataAPIKey -> Request
getTokenRequest pd req authToken =
  req {
      path = path req `combinePathSafe` encodePathSegmentsBS (toPath pd)
    , requestHeaders = [  (hAccept, "application/vnd.ambiata.v1+json")
                        , tokenHeader authToken ]
    , method = methodPost
  }
  where
    toPath Upload = ["upload", "s3"]
    toPath Download = ["download", "s3"]

combinePathSafe :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString
combinePathSafe a b =
  (BS8.reverse . BS8.dropWhile (== '/') . BS8.reverse) a <> "/" <> BS8.dropWhile (== '/') b

data PermissionDirection = Upload | Download
  deriving (Eq, Show)
