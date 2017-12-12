{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Api (
    AmbiataAPICredential (..)
  , TSRPKey
  , KeyId
  , RequestExpiry (..)

  , AmbiataAPIEndpoint (..)
  , Organisation (..)
  , ApiError (..)
  , renderApiError
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Types (Status, statusMessage)
import           Network.HTTP.Client (HttpException (..))

import           P

import           Zodiac.HttpClient (TSRPKey, KeyId, RequestExpiry(..))
import qualified Zodiac.HttpClient as Z

data AmbiataAPICredential =
    AmbiataAPIKey !Text
  | TSRPCredential !KeyId !TSRPKey !RequestExpiry
  deriving (Eq, Show)

newtype AmbiataAPIEndpoint = AmbiataAPIEndpoint {
    unAmbEndpoint :: Text
  } deriving (Eq, Show)

newtype Organisation =
  Organisation {
      unOrganisation :: Text
    } deriving (Eq, Show)

data ApiError =
    BadResponse Status
  | DecodeError Text
  | NetworkException HttpException
  | CredentialError Z.RequestError
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
    CredentialError ce ->
      "Error signing request: " <> Z.renderRequestError ce
