{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Api.Download (
    obtainCredentialsForDownload
  ) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Http
import qualified Ambiata.Cli.Json.Download as JD
import           Ambiata.Cli.Rest

import           P

import           Network.HTTP.Client
import           Network.HTTP.Types


obtainCredentialsForDownload :: Organisation -> Endpoint -> ApiRequest DownloadAccess
obtainCredentialsForDownload (Organisation o) (Endpoint e) =
  ApiRequest
    defaultRequest {
      path = encodePathSegmentsBS ["organisation", o, "download", "s3", e]
    , method = methodGet
    }
    $ \resp -> \case
      Status 200 _ ->
        JD.responseJsonV1 <$> decodeJson resp
      s ->
        Left $ BadResponse s
