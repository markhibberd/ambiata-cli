{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ambiata.Cli.Json (
    toAddress
  , toAccess
  , toSecret
  , toToken'
  , toText
  , addressToText
  ) where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Encoding

import           P

import           Network.AWS.Data

import           Mismi.S3
import           Mismi.S3.Amazonka


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
