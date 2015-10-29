{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Api.Upload (
    obtainCredentialsForUpload
  ) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Http
import qualified Ambiata.Cli.Json.Upload as JU
import           Ambiata.Cli.Rest

import           P

import           Network.HTTP.Client
import           Network.HTTP.Types


obtainCredentialsForUpload :: ApiRequest UploadAccess
obtainCredentialsForUpload =
  ApiRequest
    defaultRequest {
      path = encodePathSegmentsBS ["upload", "s3"]
    , method = methodPost
    }
    $ \resp -> \case
      Status 200 _ ->
        UploadAccess . JU.responseJsonV1 <$> decodeJson resp
      s ->
        Left $ BadResponse s
