{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ambiata.Cli.Http (
    httpGo
  , httpRetryPolicy
  , encodePathSegmentsBS
  ) where

import           Blaze.ByteString.Builder (toByteString)

import           Control.Monad.Catch
import           Control.Retry

import           Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import           Data.Text

import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.HTTP.Types.URI as URI (encodePathSegments)

import           P

import           System.IO

import           Twine.Snooze


httpGo' :: Manager -> Request -> IO (Response BSL.ByteString)
httpGo' mgr req =
  httpLbs req { checkStatus = checkStatusIgnore } mgr
  where
    -- A stupid default of http-client is to throw exceptions for non-200
    checkStatusIgnore _ _ _ = Nothing

httpGo :: RetryPolicy -> Manager -> Request -> IO (Response BSL.ByteString)
httpGo rp mgr req =
    retrying rp (\_ -> pure . httpStatusRetry . responseStatus)
  . recovering rp [const httpExceptionHandler]
  $ httpGo' mgr req

httpRetryPolicy :: RetryPolicy
httpRetryPolicy =
  capDelay (toMicroseconds $ seconds 60) $ limitRetries 5 <> exponentialBackoff (toMicroseconds $ milliseconds 100)

-- | Return true for any 'HttpException'
httpExceptionHandler :: Monad m => Handler m Bool
httpExceptionHandler =
  Handler $ \case
    (_ :: HttpException) -> return True

-- | Retry on anything that's an "error"
httpStatusRetry :: Status -> Bool
httpStatusRetry (Status s _) =
  s >= 500

-- | For setting 'queryString' on http-client "Request"
encodePathSegmentsBS :: [Text] -> ByteString
encodePathSegmentsBS =
  toByteString . URI.encodePathSegments
