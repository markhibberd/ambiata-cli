{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Env (
    UploadEnv (..)
  , DownloadEnv (..)
  , CommonEnv (..)
  , uploadEnv
  , uploadEnv'
  , downloadEnv
  , downloadEnv'
  , common
  , apiCredential
  ) where

import           Ambiata.Cli.Data

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Env

import           Network.URI         ()

import           P

import           System.IO

import qualified Zodiac.HttpClient as Z

uploadEnv :: IO UploadEnv
uploadEnv = parse (header "ambiata-upload") uploadEnv'

uploadEnv' :: Parser UploadEnv
uploadEnv' = UploadEnv <$> var incomingdir "UPLOAD_DIR" (help "Directory to upload from.")
                       <*> (var retention "ARCHIVE_RETENTION" $ fold <> mempty $
                             [ def (Retention 7)
                             , helpDef (T.unpack . renderRetention)
                             , help "Days to wait before cleaning up archived uploads. A value of zero means \"never delete\"."
                             ])
                       <*> common

downloadEnv :: IO DownloadEnv
downloadEnv = parse (header "ambiata-download") downloadEnv'

downloadEnv' :: Parser DownloadEnv
downloadEnv' =
  DownloadEnv
    <$> var outgoingdir "DOWNLOAD_DIR" (help "Directory to download to.")
    <*> var (fmap Organisation <$> nonempty) "ORGANISATION_ID" (help "Organisation ID")
    <*> var (fmap Endpoint <$> nonempty)  "ENDPOINT_ID" (help "Endpoint ID to download from")
    <*> common

common :: Parser CommonEnv
common =
    CommonEnv <$> apiCredential
              <*> (var endpoint "AMBIATA_API_ENDPOINT" $ fold <> mempty $
                    [ def Nothing
                    , helpDef (const . T.unpack . unAmbEndpoint $ defaultAmbiataAPIEndpoint)
                    , help "API endpoint from which to request upload credentials."
                    ])
              <*> (var region "AMBIATA_REGION" $ fold <> mempty $
                    [ def AmbiataAu
                    , helpDef (const "au")
                    , help "Ambiata region for API access, one of 'au', 'us', 'sg', 'pii-au', 'pii-us', 'pii-sg'."
                    ])
              <*> (runmode <$> (switch "ONESHOT" (help "Don't run forever.")))
              <*> (debugVerbosity <$> (switch "DEBUG" (help "Turn on verbose logging.")))

apiCredential :: Parser AmbiataAPICredential
apiCredential =
  apiCredentialTsrp <|> apiCredentialToken

apiCredentialToken :: Parser AmbiataAPICredential
apiCredentialToken =
  var (pure . AmbiataAPIKey . T.pack) "AMBIATA_API_KEY" (help "API key token")

apiCredentialTsrp :: Parser AmbiataAPICredential
apiCredentialTsrp =
  TSRPCredential
    <$> var tsrpKeyId "AMBIATA_API_KEY_ID" (help "API key ID.")
    <*> var tsrpKey "AMBIATA_API_KEY_SECRET" (help "API key secret.")
    <*> var requestExpiry "AMBIATA_API_REQUEST_EXPIRY" (
             help "Seconds before request expires. You shouldn't need to change this."
          <> def (RequestExpiry 60)
          <> helpDef (show . unRequestExpiry))

requestExpiry :: Reader RequestExpiry
requestExpiry =
  auto >=> \x -> case x > 0 of
    True -> pure $ RequestExpiry x
    False -> Left "Request expiry must be positive."

tsrpKey :: Reader TSRPKey
tsrpKey =
  nonempty >=>
    (maybeToRight "Provided key secret is invalid." . lazyMaybe' . Z.parseTSRPKey . T.encodeUtf8)

tsrpKeyId :: Reader KeyId
tsrpKeyId =
  nonempty >=>
    (maybeToRight "Provided key ID is invalid." . lazyMaybe' . Z.parseKeyId . T.encodeUtf8)

incomingdir :: Reader IncomingDir
incomingdir = nonempty >=> (Right . IncomingDir)

outgoingdir :: Reader DownloadDir
outgoingdir = nonempty >=> (Right . DownloadDir)

endpoint :: Reader (Maybe AmbiataAPIEndpoint)
endpoint = maybeR AmbiataAPIEndpoint

maybeR :: (Text -> a) -> Reader (Maybe a)
maybeR f = (fmap . fmap) (Just . f . T.pack) nonempty

region :: Reader AmbiataRegion
region = nonempty >=> str >=> (maybeToRight "Invalid specification for AMBIATA_REGION must be 'au' or 'us'." . parseAmbiataRegion)


runmode :: Bool -> RunMode
runmode True = OneShot
runmode False = Daemon

debugVerbosity :: Bool -> Verbosity
debugVerbosity True  = Verbose
debugVerbosity False = Quiet

retention :: Reader Retention
retention s = case readMaybe s of
  Nothing -> Left "Must be integral."
  Just n -> if n >= 0 then Right (Retention n) else Left "Must be positive."
