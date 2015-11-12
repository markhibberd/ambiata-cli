{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Api (
    AmbiataAPIKey (..)
  , AmbiataAPIEndpoint (..)
  , Organisation (..)
  , ApiError (..)
  , renderApiError
  , tokenHeader
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Types (Header, Status, hAuthorization, statusMessage)
import           Network.HTTP.Client (HttpException (..))

import           P


newtype AmbiataAPIKey = AmbiataAPIKey {
    unAmbKey :: Text
  } deriving (Eq, Show)

newtype AmbiataAPIEndpoint = AmbiataAPIEndpoint {
    unAmbEndpoint :: Text
  } deriving (Show)

newtype Organisation =
  Organisation {
      unOrganisation :: Text
    } deriving (Eq, Show)

data ApiError =
    BadResponse Status
  | DecodeError Text
  | NetworkException HttpException
  deriving Show


renderApiError :: ApiError -> Text
renderApiError e =
  case e of
    BadResponse t ->
      "An error occurred with the Ambiata API: " <> (T.decodeUtf8 . statusMessage) t
    DecodeError t ->
      "Incorrect JSON returned from Ambiata API: " <> t
    NetworkException t ->
      "A network error has occured with the Ambiata API: " <> (T.pack $ show t)

tokenHeader :: AmbiataAPIKey -> Header
tokenHeader (AmbiataAPIKey k) =
  (hAuthorization, T.encodeUtf8 $ "token " <> k)
