{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ambiata.Cli.Data.Transfer (
    TemporaryCreds (..)
  , Endpoint (..)
  ) where

import           Data.Text (Text)

import           Mismi.S3.Amazonka (AccessKey, SecretKey (..), SessionToken (..))

import           P


data TemporaryCreds =
  TemporaryCreds {
    tempKey      :: AccessKey,
    tempSecret   :: SecretKey,
    sessionToken :: SessionToken
  } deriving (Eq, Show)

instance Show SecretKey where
  show (SecretKey bs)  = show bs

instance Show SessionToken where
  show (SessionToken bs) = show bs

newtype Endpoint =
  Endpoint {
      unEndpoint :: Text
    } deriving (Eq, Show)
