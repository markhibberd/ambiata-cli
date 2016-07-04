{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Ambiata.Cli.Data where

import           Ambiata.Cli.Data

import           Disorder.Core

import           Mismi (Region (..))

import           P                          hiding (isPrefixOf)

import           System.IO

import           Data.Text

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck

prop_filepath :: IncomingDir -> IncomingFile -> Bool
prop_filepath d f =
  (unIncomingFile f) `isSuffixOf` (pack $ toFilePath d f)

prop_dir :: IncomingDir -> IncomingFile -> Bool
prop_dir d f =
  (pack $ unDir d) `isPrefixOf` (pack $ toFilePath d f)

prop_working :: IncomingDir -> WorkingDir -> Bool
prop_working d w =
  (pack $ unDir d) `isPrefixOf` (pack $ toWorkingPath d w)

prop_working_path :: IncomingDir -> Bool
prop_working_path d =
  ("/archive") `isSuffixOf` (pack $ toWorkingPath d Archive)

prop_working_processing :: IncomingDir -> Bool
prop_working_processing d =
  ("/.processing") `isSuffixOf` (pack $ toWorkingPath d Processing)

prop_tripping_ambiata_region :: AmbiataRegion -> Property
prop_tripping_ambiata_region =
  tripping renderAmbiataRegion parseAmbiataRegion


-- these are here as explicit compatibility cross-checks, changing
-- these means we have to change doc, so careful if you are editing
-- around here...

prop_compatibility_default_region :: Property
prop_compatibility_default_region =
  defaultAmbiataRegion === AmbiataAu

prop_aws_region_explicit_au :: Property
prop_aws_region_explicit_au =
  toAWSRegion AmbiataAu === Sydney

prop_aws_region_explicit_us :: Property
prop_aws_region_explicit_us =
  toAWSRegion AmbiataUs === NorthVirginia

prop_endpoint_explicit_au :: Property
prop_endpoint_explicit_au =
  defaultAmbiataAPIEndpointFor AmbiataAu === AmbiataAPIEndpoint "https://api.ambiata.com"

prop_endpoint_explicit_us :: Property
prop_endpoint_explicit_us =
  defaultAmbiataAPIEndpointFor AmbiataUs === AmbiataAPIEndpoint "https://us-api.ambiata.com"


return []
tests :: IO Bool
tests = $quickCheckAll
