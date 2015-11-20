{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data (
    module X
  , AmbiataError (..)
  , renderClientError
  ) where

import           Ambiata.Cli.Data.Api as X
import           Ambiata.Cli.Data.Download as X
import           Ambiata.Cli.Data.Env as X
import           Ambiata.Cli.Data.File as X
import           Ambiata.Cli.Data.Upload as X

import           Mismi (Error, renderError)

import           P

import           Data.Text (Text)


data AmbiataError =
    AmbiataApiError ApiError
  | AmbiataAWSUploadError Error
  | AmbiataAWSDownloadError Error
  | AmbiataFilesystemError FilesystemError
  | AmbiataDownloadError DownloadError

renderClientError :: AmbiataError -> Text
renderClientError ae =
  case ae of
    AmbiataApiError e ->
      renderApiError e
    AmbiataAWSUploadError a ->
      "An error occured whilst uploading with the AWS API: " <> renderError a
    AmbiataAWSDownloadError a ->
      "An error occured whilst downloading with the AWS API: " <> renderError a
    AmbiataFilesystemError e ->
      renderFilesystemError e
    AmbiataDownloadError e ->
      "An error occured whilst downloading: " <> renderDownloadError e
