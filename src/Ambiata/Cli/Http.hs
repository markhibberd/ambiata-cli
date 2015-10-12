{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ambiata.Cli.Http (
    ErrorCount (..)
  , httpGo
  , httpRetryPolicy
  , encodePathSegmentsBS
  ) where

import           Blaze.ByteString.Builder (toByteString)

import           Control.Monad.Catch
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
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


newtype ErrorCount =
  ErrorCount {
    unErrorCount :: Int
  } deriving (Eq, Show)

instance Monoid ErrorCount where
  mempty = ErrorCount 0
  mappend (ErrorCount a) (ErrorCount b) = ErrorCount $ a + b

httpGo' :: Manager -> Request -> IO (Response BSL.ByteString)
httpGo' mgr req =
  httpLbs req { checkStatus = checkStatusIgnore } mgr
  where
    -- A stupid default of http-client is to throw exceptions for non-200
    checkStatusIgnore _ _ _ = Nothing

httpGo :: RetryPolicy -> Manager -> Request -> IO (Response BSL.ByteString, ErrorCount)
httpGo rp mgr req =
    runWriterT
  . retrying rp (const $ countError . pure . httpStatusRetry . responseStatus)
  . recovering rp [const httpExceptionHandler]
  . lift
  $ httpGo' mgr req

httpRetryPolicy :: RetryPolicy
httpRetryPolicy =
  capDelay (toMicroseconds $ seconds 60) $ limitRetries 10 <> exponentialBackoff (toMicroseconds $ milliseconds 200)

-- | Return true for any 'HttpException'
httpExceptionHandler :: Monad m => Handler (WriterT ErrorCount m) Bool
httpExceptionHandler =
  Handler $ \case
    (_ :: HttpException) -> countError $ return True

-- | Retry on anything that's an "error"
httpStatusRetry :: Status -> Bool
httpStatusRetry (Status s _) =
  s >= 500

-- | For setting 'queryString' on http-client "Request"
encodePathSegmentsBS :: [Text] -> ByteString
encodePathSegmentsBS =
  toByteString . URI.encodePathSegments

countError :: Monad m => m Bool -> WriterT ErrorCount m Bool
countError m =
  lift m >>= \b -> do
    when b $ tell (ErrorCount 1)
    return b
