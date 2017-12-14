{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This deals with files that are ready to ingest.
--

module Ambiata.Cli.Rest (
    ApiRequest (..)
  , apiCall
  , decodeJson
  , combinePathSafe
  , defaultRequest
  ) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Http

import           P

import           System.IO

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import           Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Control.Monad.Catch        (catch)
import           Control.Monad.IO.Class     (liftIO)

import           Data.Aeson

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types

import           X.Control.Monad.Trans.Either

import           Zodiac.HttpClient (RequestTimestamp)
import qualified Zodiac.HttpClient as Z


data ApiRequest a =
  ApiRequest {
    apiRequest :: Request
  , apiResponse :: Response LBS.ByteString -> Status -> Either ApiError a
  }


apiCall ::
     AmbiataAPICredential
  -> AmbiataAPIEndpoint
  -> ApiRequest a
  -> EitherT ApiError IO a
apiCall cred ep (ApiRequest apiReq resp) = do
  now <- liftIO Z.timestampRequest
  m <- liftIO $ newManager tlsManagerSettings
  req <- liftIO $ setRequestEndpoint ep apiReq
  req' <- hoistEither . first CredentialError $ addRequestHeaders now cred req
  (res, _) <- (liftIO (httpGo httpRetryPolicy m req')) `catch` (\(e :: HttpException) -> left $ NetworkException e)
  hoistEither $ resp res (responseStatus res)

decodeJson :: FromJSON a => Response LBS.ByteString -> Either ApiError a
decodeJson res =
  first (DecodeError . T.pack) . eitherDecode $ responseBody res

addRequestHeaders :: RequestTimestamp -> AmbiataAPICredential -> Request -> Either Z.RequestError Request
addRequestHeaders now (TSRPCredential kid sk re) req =
  flip (Z.authedHttpClientRequest kid sk re) now $ req {
      requestHeaders = requestHeaders req <> [
          (hAccept, apiVersion)
        ]
    }
addRequestHeaders _ (AmbiataAPIKey k) req =
  pure req {
      requestHeaders =
        (hAuthorization, T.encodeUtf8 $ "token " <> k) : requestHeaders req
    }

-- FIXUP AmbiataAPIEndpoint should ideally contain 'URI' which would make this a little nicer without errors
setRequestEndpoint :: AmbiataAPIEndpoint -> Request -> IO Request
setRequestEndpoint (AmbiataAPIEndpoint ep) req = do
  req' <- parseUrl $ T.unpack ep
  pure $ req {
      host = host req'
    , port = port req'
    , secure = secure req'
    , queryString = queryString req'
    , path = path req' `combinePathSafe` path req
    }

apiVersion :: ByteString
apiVersion =
  "application/vnd.ambiata.v1+json"

combinePathSafe :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString
combinePathSafe a b =
  (BS8.reverse . BS8.dropWhile (== '/') . BS8.reverse) a <> "/" <> BS8.dropWhile (== '/') b

defaultRequest :: Request
defaultRequest =
  def
