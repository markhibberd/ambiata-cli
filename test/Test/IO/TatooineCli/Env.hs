{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.TatooineCli.Env where


import           P

import           Test.QuickCheck

import           System.IO
import           System.Posix.Env

import           Data.Text

import           Disorder.Core.IO
import           Disorder.Corpus

import           TatooineCli.Data
import           TatooineCli.Env

import           Control.Monad.IO.Class         (liftIO)

import           Test.IO.TatooineCli.Processing (expectLeft, runOrFail)
import           Test.TatooineCli.Arbitrary     ()

import           Control.Monad.Trans.Either

import           Snooze.Url


prop_load_failure :: Property
prop_load_failure = testIO . runOrFail $ do
  liftIO $ unsetEnv "AMBIATA_API_KEY"
  expectLeft "Should not have loaded environment" environment >> pure True

prop_load_environment :: TestEnv -> Property
prop_load_environment (TestEnv ep' key' dir' dl') = testIO $ do
  _ <- setEnv "AMBIATA_API_ENDPOINT" (unpack ep') True
  _ <- setEnv "AMBIATA_API_KEY" (unpack key') True
  _ <- setEnv "UPLOAD_DIR" (unpack dir') True
  _ <- setEnv "DOWNLOAD_DIR" (unpack dl') True
  Right (Env incoming apikey (AmbiataAPIEndpoint endpoint) dl'') <- runEitherT environment
  pure $ (incoming, apikey, show $ Just endpoint, dl'') ===
         (IncomingDir $ unpack dir', AmbiataAPIKey key', show url', DownloadDir dl')
  where
    url' = url ep' (pathRaw "")


data TestEnv =
  TestEnv {
  apiEndpoint :: Text,
  apiKey      :: Text,
  uploadDir   :: Text,
  downloadDir :: Text
} deriving (Show)

instance Arbitrary TestEnv where
  arbitrary = TestEnv <$> elements (fmap ("https://" <>) muppets)
                      <*> elements cooking
                      <*> elements muppets


return []
tests :: IO Bool
tests = $quickCheckAll
