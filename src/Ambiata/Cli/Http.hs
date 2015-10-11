{-# LANGUAGE NoImplicitPrelude #-}
module Ambiata.Cli.Http (
    httpGo
  , encodePathSegmentsBS
  ) where

import           Blaze.ByteString.Builder (toByteString)

import           Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import           Data.Text

import           Network.HTTP.Client
import           Network.HTTP.Types.URI as URI (encodePathSegments)

import           P

import           System.IO


httpGo :: Manager -> Request -> IO (Response BSL.ByteString)
httpGo mgr req =
  httpLbs req { checkStatus = checkStatusIgnore } mgr
  where
    -- A stupid default of http-client is to throw exceptions for non-200
    checkStatusIgnore _ _ _ = Nothing

-- | For setting 'queryString' on http-client "Request"
encodePathSegmentsBS :: [Text] -> ByteString
encodePathSegmentsBS =
  toByteString . URI.encodePathSegments
