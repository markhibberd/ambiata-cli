{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Ambiata.Cli.Env where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Env

import qualified Data.ByteString.Char8 as BSC
import           Data.Text
import qualified Data.Text.Encoding as T

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
prop_load_common_environment te = testIO $ do
  setTestEnv te
  x <- E.parseOr pure mempty common
  pure $ (
      fmap (fmap unAmbEndpoint . explicitApiEndpoint) x
    ) === (
      Right (Just $ tapiEndpoint te)
    )

prop_load_upload_environment :: TestEnv -> Property
prop_load_upload_environment te = testIO $ do
  setTestEnv te
  x <- E.parseOr pure mempty uploadEnv'
  pure $ (
      fmap (\(UploadEnv dir'' _ _) -> pack $ unDir dir'') x
    , fmap (\(UploadEnv _ rt _) -> renderIntegral $ retentionDays rt) x
    ) === (
      Right $ tuploadDir te
    , Right $ tarchiveRetention te
    )

prop_load_download_environment :: TestEnv -> Property
prop_load_download_environment te = testIO $ do
  setTestEnv te
  x <- E.parseOr pure mempty downloadEnv'
  pure $ (
      fmap (\(DownloadEnv dd _ _ _) -> pack $ unDownloadDir dd) x
    , fmap (\(DownloadEnv _ org _ _) -> unOrganisation org) x
    , fmap (\(DownloadEnv _ _ e _) -> unEndpoint e) x
    ) === (
      Right $ tdownloadDir te
    , Right $ torganisationId te
    , Right $ tendpointId te
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

setTestEnv :: TestEnv -> IO ()
setTestEnv (TestEnv ep' dir' dl' rt oid eid kid sk re) =
  mapM_ (\(k,v) -> setEnv k (unpack v) True) [
      ("AMBIATA_API_ENDPOINT", ep')
    , ("AMBIATA_API_KEY_ID", kid)
    , ("AMBIATA_API_KEY_SECRET", sk)
    , ("AMBIATA_API_REQUEST_EXPIRY", re)
    , ("UPLOAD_DIR", dir')
    , ("DOWNLOAD_DIR", dl')
    , ("ARCHIVE_RETENTION", rt)
    , ("ORGANISATION_ID", oid)
    , ("ENDPOINT_ID", eid)
    ]

data TestEnv =
  TestEnv {
    tapiEndpoint :: Text
  , tuploadDir :: Text
  , tdownloadDir :: Text
  , tarchiveRetention :: Text
  , torganisationId :: Text
  , tendpointId :: Text
  , tapiKeyId :: Text
  , tapiKeySecret :: Text
  , tapiRequestExpiry :: Text
  } deriving (Show)

instance Arbitrary TestEnv where
  arbitrary = TestEnv <$> elements (fmap ("https://" <>) southpark)
                      <*> elements muppets
                      <*> elements viruses
                      <*> (renderIntegral <$> (choose (0 :: Int, 20)))
                      <*> elements colours
                      <*> elements weather
                      <*> ((T.decodeUtf8 . Z.renderKeyId) <$> arbitrary)
                      <*> ((T.decodeUtf8 . Z.renderTSRPKey) <$> arbitrary)
                      <*> (renderIntegral <$> (choose (0 :: Int, maxBound)))


return []
tests :: IO Bool
tests = $quickCheckAll
