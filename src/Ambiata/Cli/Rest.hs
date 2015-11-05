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

import           Control.Monad.Catch        (catch)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either

import           Data.Aeson

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types


data ApiRequest a =
  ApiRequest {
    apiRequest :: Request
  , apiResponse :: Response LBS.ByteString -> Status -> Either ApiError a
  }


apiCall ::
     AmbiataAPIKey
  -> AmbiataAPIEndpoint
  -> ApiRequest a
  -> EitherT ApiError IO a
apiCall key' ep (ApiRequest apiReq resp) = do
  m <- liftIO $ newManager tlsManagerSettings
  req <- liftIO $ setRequestEndpoint ep apiReq
  (res, _) <- (liftIO . httpGo httpRetryPolicy m $ addRequestHeaders req key') `catch` (\(e :: HttpException) -> left $ NetworkException e)
  hoistEither $ resp res (responseStatus res)

decodeJson :: FromJSON a => Response LBS.ByteString -> Either ApiError a
decodeJson res =
  first (DecodeError . T.pack) . eitherDecode $ responseBody res

addRequestHeaders :: Request -> AmbiataAPIKey -> Request
addRequestHeaders req authToken =
  req {
      requestHeaders = requestHeaders req <> [
          (hAccept, apiVersion)
        , tokenHeader authToken
        ]
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
