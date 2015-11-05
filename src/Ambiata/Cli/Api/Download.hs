{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Api.Download (
    obtainCredentialsForDownload
  ) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Http
import qualified Ambiata.Cli.Json.Upload as JU
import           Ambiata.Cli.Rest

import           P

import           Network.HTTP.Client
import           Network.HTTP.Types


-- FIXUP This API is incorrect, and isn't being used at the moment
obtainCredentialsForDownload :: ApiRequest DownloadAccess
obtainCredentialsForDownload =
  ApiRequest
    defaultRequest {
      path = encodePathSegmentsBS ["download", "s3"]
    , method = methodPost
    }
    $ \resp -> \case
      Status 200 _ ->
        DownloadAccess . JU.responseJsonV1 <$> decodeJson resp
      s ->
        Left $ BadResponse s
