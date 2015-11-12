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
  ) where

import           Ambiata.Cli.Data

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text           as T

import           Env

import           Network.URI         ()

import           P

import           System.IO

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
    <*> var (fmap Organisation <$> nonemptyStr) "ORGANISATION_ID" (help "Organisation ID")
    <*> var (fmap Endpoint <$> nonemptyStr)  "ENDPOINT_ID" (help "Endpoint ID to download from")
    <*> common

common :: Parser CommonEnv
common =
    CommonEnv <$> var appToken "AMBIATA_API_KEY" (help "Application token for Ambiata API.")
              <*> (var endpoint "AMBIATA_API_ENDPOINT" $ fold <> mempty $
                    [ def (AmbiataAPIEndpoint "https://api.ambiata.com")
                    , helpDef (T.unpack . unAmbEndpoint)
                    , help "API endpoint from which to request upload credentials."
                    ])
              <*> (runmode <$> (switch "ONESHOT" (help "Don't run forever.")))
              <*> (debugVerbosity <$> (switch "DEBUG" (help "Turn on verbose logging.")))

incomingdir :: Reader IncomingDir
incomingdir = str >=> nonempty >=> (Right . IncomingDir)

outgoingdir :: Reader DownloadDir
outgoingdir = str >=> nonempty >=> (Right . DownloadDir)

nonemptyStr :: Reader Text
nonemptyStr = str >=> nonempty

endpoint :: Reader AmbiataAPIEndpoint
endpoint = (fmap . fmap) (AmbiataAPIEndpoint . T.pack) $ (nonempty >=> str)

runmode :: Bool -> RunMode
runmode True = OneShot
runmode False = Daemon

appToken :: Reader AmbiataAPIKey
appToken = str >=> nonempty >=> (Right . AmbiataAPIKey . T.pack)

debugVerbosity :: Bool -> Verbosity
debugVerbosity True  = Verbose
debugVerbosity False = Quiet

retention :: Reader Retention
retention s = case readMaybe s of
  Nothing -> Left "Must be integral."
  Just n -> if n >= 0 then Right (Retention n) else Left "Must be positive."
