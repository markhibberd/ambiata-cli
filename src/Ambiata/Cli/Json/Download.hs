{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Json.Download (
    ResponseJsonV1 (..)
  ) where


import           Ambiata.Cli.Data.Download
import           Ambiata.Cli.Json

import           Data.Aeson
import           Data.Aeson.Types

import           P


data ResponseJsonV1  =
  ResponseJsonV1 {
    responseJsonV1 :: DownloadAccess
  } deriving (Eq, Show)


instance ToJSON ResponseJsonV1 where
  toJSON (ResponseJsonV1 (DownloadAccess (TemporaryCreds ak sk st) a)) =
    object [
        "access_key" .= toText ak
      , "access_secret" .= toText sk
      , "session_token" .= toText st
      , "s3_paths" .= fmap (addressToText . unServerFile) a
      ]

instance FromJSON ResponseJsonV1 where
  parseJSON (Object o) =
    fmap ResponseJsonV1 $ DownloadAccess
      <$> (TemporaryCreds
        <$> (o .: "access_key" >>= toAccess)
        <*> (o .: "access_secret" >>= toSecret)
        <*> (o .: "session_token" >>= toToken')
        )
      <*> (o .: "s3_paths" >>= traverse ((=<<) createServerFileOrFail . toAddress))
  parseJSON x =
    typeMismatch "Not a valid Download.ResponseJsonV1 object" x
