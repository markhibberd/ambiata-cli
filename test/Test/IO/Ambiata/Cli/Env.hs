{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Ambiata.Cli.Env where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Env

import qualified Data.ByteString.Char8 as BSC
import           Data.Text

import           Disorder.Core (failWith)
import           Disorder.Core.IO
import           Disorder.Corpus

import qualified Env as E

import           P

import           System.IO
import           System.Posix.Env

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck

import qualified Zodiac.TSRP.Data as Z

prop_load_failure :: Property
prop_load_failure = testIO $ do
  unsetEnv "AMBIATA_API_KEY"
  x <- E.parseOr pure mempty common
  pure $ either (const (True === True)) (\e -> failWith $ "Should not have loaded environment: " <> pack (show e)) x

prop_load_common_environment :: TestEnv -> Property
prop_load_common_environment (TestEnv ep' key' _dir' _dl') = testIO $ do
  _ <- setEnv "AMBIATA_API_ENDPOINT" (unpack ep') True
  _ <- setEnv "AMBIATA_API_KEY" (unpack key') True
  x <- E.parseOr pure mempty common
  pure $ (
      fmap (fmap unAmbEndpoint . explicitApiEndpoint) x
    , fmap (unAmbKey . apiKey) x
    ) === (
      Right (Just ep')
    , Right key'
    )

prop_api_credential :: AmbiataAPICredential -> Property
prop_api_credential c@(TSRPCredential kid sk re) =
  let
    kid' = BSC.unpack $ Z.renderKeyId kid
    sk' = BSC.unpack $ Z.renderTSRPKey sk
    re' = BSC.unpack $ Z.renderRequestExpiry re
  in testIO $ do
  setEnv "AMBIATA_API_KEY_ID" kid' True
  setEnv "AMBIATA_API_KEY_SECRET" sk' True
  setEnv "AMBIATA_API_REQUEST_EXPIRY" re' True
  c' <- E.parse mempty apiCredential
  pure $ c === c'

data TestEnv =
  TestEnv {
  tapiEndpoint :: Text,
  tapiKey      :: Text,
  tuploadDir   :: Text,
  tdownloadDir :: Text
} deriving (Show)

instance Arbitrary TestEnv where
  arbitrary = TestEnv <$> elements (fmap ("https://" <>) southpark)
                      <*> elements cooking
                      <*> elements muppets
                      <*> elements viruses


return []
tests :: IO Bool
tests = $quickCheckAll
