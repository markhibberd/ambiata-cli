{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Ambiata.Cli.Credentials where

import           Ambiata.Cli.Credentials
import           Ambiata.Cli.Data
import           Ambiata.Cli.Json

import           P

import           System.IO

import           Disorder.Core.IO

import           Data.Aeson
import qualified Data.Text as T
import           Data.Text.Encoding

import           Control.Monad.Trans.Either

import           Network.HTTP.Client

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import           Test.Snooze.Arbitrary      ()
import           Test.Snooze.Server

import           Web.Scotty                 as S

prop_upload_creds :: AmbiataAPIKey -> TemporaryAccess -> Property
prop_upload_creds k ta = testIO $ do
  let bs = encode ta
  ta' <- withServer (S.post (regex "/.*") $ S.raw bs) $ \u ->
    runEitherT $ obtainCredentialsForUpload k (AmbiataAPIEndpoint $ "http://" <> (decodeUtf8 $ host u) <> ":" <> (T.pack . show $ port u))
  pure $ ta' === Right (UploadAccess ta)



return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
