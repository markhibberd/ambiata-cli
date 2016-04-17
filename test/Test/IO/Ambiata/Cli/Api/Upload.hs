{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Ambiata.Cli.Api.Upload where

import           Ambiata.Cli.Api.Upload
import           Ambiata.Cli.Rest
import           Ambiata.Cli.Data
import qualified Ambiata.Cli.Json.Upload as JU

import           P

import           System.IO

import           Disorder.Core.IO

import           Data.Aeson
import qualified Data.Text as T
import           Data.Text.Encoding

import           Network.HTTP.Client

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.IO.Ambiata.Cli.Http
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import           Web.Scotty                 as S

import           X.Control.Monad.Trans.Either

prop_upload_creds :: AmbiataAPIKey -> TemporaryAccess -> Property
prop_upload_creds k ta = testIO $ do
  ta' <- withServer (S.post (regex "/.*") . S.raw . encode . JU.ResponseJsonV1 $ ta) $ \u ->
    runEitherT
      . apiCall k (AmbiataAPIEndpoint $ "http://" <> (decodeUtf8 $ host u) <> ":" <> (T.pack . show $ port u))
      $ obtainCredentialsForUpload
  r <- either (fail . T.unpack . (<>) "Upload creds failed with: " . renderApiError) pure ta'
  pure $ r === UploadAccess ta



return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
