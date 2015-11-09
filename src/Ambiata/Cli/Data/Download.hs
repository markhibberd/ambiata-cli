{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Download (
    module X
  , DownloadDir (..)
  , DownloadResult (..)
  , DownloadAccess (..)
  , DownloadEnv (..)
  , ServerFile (..)
  , renderDownloadResult
  ) where

import           Ambiata.Cli.Data.Env
import           Ambiata.Cli.Data.File
import           Ambiata.Cli.Data.Transfer as X
import           Ambiata.Cli.Data.Upload

import           Data.Text (Text)

import           P

import           System.IO (FilePath)


newtype DownloadDir = DownloadDir {
  unDownloadDir :: FilePath
} deriving (Show, Eq, Ord)

newtype DownloadResult = DownloadResult { unDownloadResult :: [LocalFile] }
  deriving (Show, Eq, Ord)

newtype DownloadAccess = DownloadAccess { unDownloadAccess :: TemporaryAccess }
  deriving (Eq, Show)

data DownloadEnv =
  DownloadEnv DownloadDir CommonEnv deriving (Show)

newtype ServerFile = ServerFile {
  unServerFile :: Text
} deriving (Show, Eq, Ord)


renderDownloadResult :: DownloadResult -> Text
renderDownloadResult (DownloadResult fs) =
  "Downloaded files: " <> renderFileList (unLocalFile <$> fs)
