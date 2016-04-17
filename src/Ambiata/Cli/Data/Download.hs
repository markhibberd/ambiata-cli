{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Download (
    module X
  , DownloadDir (..)
  , DownloadResult (..)
  , DownloadAccess (..)
  , DownloadEnv (..)
  , ServerFile
  , DownloadError (..)
  , renderDownloadResult
  , unServerFile
  , createServerFile
  , createServerFileOrFail
  , serverFilePath
  , renderDownloadError
  ) where

import           Ambiata.Cli.Data.Api
import           Ambiata.Cli.Data.Env
import           Ambiata.Cli.Data.File
import           Ambiata.Cli.Data.Transfer as X

import qualified Data.Text as T

import           Mismi.S3 (Address, addressToText, basename, key)

import           P

import           System.IO (FilePath)


newtype DownloadDir = DownloadDir {
  unDownloadDir :: FilePath
} deriving (Show, Eq, Ord)

newtype DownloadResult = DownloadResult { unDownloadResult :: [LocalFile] }
  deriving (Show, Eq, Ord)

data DownloadAccess =
  DownloadAccess {
      downloadTemporaryCred :: TemporaryCreds
    , downloadPaths :: [ServerFile]
    } deriving (Eq, Show)

data DownloadEnv =
  DownloadEnv DownloadDir Organisation Endpoint CommonEnv deriving (Show)

-- |
-- Represents a file to be downloaded from the API.
-- Cannot be "prefix" address (eg. s3://bucket/).
-- Use 'createServerFile' as the constructor.
--
newtype ServerFile = ServerFile {
  _unServerFile :: Address
} deriving (Show, Eq, Ord)

data DownloadError =
    ServerFileDoesNotExist ServerFile
  deriving (Show)


renderDownloadResult :: DownloadResult -> Text
renderDownloadResult (DownloadResult fs) =
  "Downloaded files: " <> renderFileList (unLocalFile <$> fs)

unServerFile :: ServerFile -> Address
unServerFile =
  _unServerFile

createServerFile :: Address -> Maybe ServerFile
createServerFile a =
  fmap (const (ServerFile a)) . basename . key $ a

createServerFileOrFail :: Monad m => Address -> m ServerFile
createServerFileOrFail a =
  maybe (fail . T.unpack $ "Invalid ServerFile: " <> addressToText a) return . createServerFile $ a

serverFilePath :: ServerFile -> FilePath
serverFilePath =
  maybe "" T.unpack . basename . key . unServerFile

renderDownloadError :: DownloadError -> Text
renderDownloadError ae =
  case ae of
    ServerFileDoesNotExist (ServerFile a) ->
      "File to download does not exist: " <> addressToText a
