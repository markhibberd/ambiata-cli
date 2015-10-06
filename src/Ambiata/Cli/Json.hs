{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ambiata.Cli.Json (
  TemporaryAccess(..),
  TemporaryCreds(..)
) where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Encoding


import           P

import           Network.AWS.Data

import           Mismi.S3
import           Mismi.S3.Amazonka

data TemporaryCreds =
  TemporaryCreds {
    tempKey      :: AccessKey,
    tempSecret   :: SecretKey,
    sessionToken :: SessionToken
  } deriving (Eq, Show)

data TemporaryAccess =
  TemporaryAccess {
    tempCreds :: TemporaryCreds,
    s3Path    :: Address
  } deriving (Eq, Show)




instance Show SecretKey where
  show (SecretKey bs)  = show bs

instance Show SessionToken where
  show (SessionToken bs) = show bs


instance ToJSON TemporaryAccess where
  toJSON (tt) =
    object [  "access_key" .= (toText $ tempKey creds)
            , "access_secret" .= (toText $ tempSecret creds)
            , "session_token" .= (toText $ sessionToken creds)
            , "s3_path" .= addressToText (s3Path tt)
         ]
    where
      creds = tempCreds tt

instance FromJSON TemporaryAccess where
  parseJSON (Object o) = TemporaryAccess
                            <$> (TemporaryCreds <$> (o .: "access_key" >>= toAccess)
                                                <*> (o .: "access_secret" >>= toSecret)
                                                <*> (o .: "session_token" >>= toToken'))
                            <*> (o .: "s3_path" >>= toAddress)
  parseJSON x =  typeMismatch "Not a valid TemporaryAccess object" x


toAddress :: Value -> Parser Address
toAddress (String s) =
    maybe (fail "s3_path must be a valid s3 address") pure $ addressFromText s
toAddress _ = fail ("s3_path must be a string and a valid s3 address")


toAccess :: Value -> Parser AccessKey
toAccess (String s) =
    pure $ AccessKey (encodeUtf8 s)
toAccess _ = fail ("aws_key must be a string")


toSecret :: Value -> Parser SecretKey
toSecret (String s) =
    pure $ SecretKey (encodeUtf8 s)
toSecret _ = fail ("aws_secret must be a string")


toToken' :: Value -> Parser SessionToken
toToken' (String s) =
    pure $ SessionToken (encodeUtf8 s)
toToken' _ = fail ("aws_session_token must be a string")
