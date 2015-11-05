{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ambiata.Cli.Json.Upload (
  ResponseJsonV1(..)
) where


import           Ambiata.Cli.Json

import           Data.Aeson
import           Data.Aeson.Types

import           P


data ResponseJsonV1  =
  ResponseJsonV1 {
    responseJsonV1 :: TemporaryAccess
  } deriving (Eq, Show)


instance ToJSON ResponseJsonV1 where
  toJSON (ResponseJsonV1 tt) =
    object [  "access_key" .= (toText $ tempKey creds)
            , "access_secret" .= (toText $ tempSecret creds)
            , "session_token" .= (toText $ sessionToken creds)
            , "s3_path" .= addressToText (s3Path tt)
         ]
    where
      creds = tempCreds tt

instance FromJSON ResponseJsonV1 where
  parseJSON (Object o) = fmap ResponseJsonV1 $ TemporaryAccess
                            <$> (TemporaryCreds <$> (o .: "access_key" >>= toAccess)
                                                <*> (o .: "access_secret" >>= toSecret)
                                                <*> (o .: "session_token" >>= toToken'))
                            <*> (o .: "s3_path" >>= toAddress)
  parseJSON x =  typeMismatch "Not a valid TemporaryAccess object" x
