{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Ambiata.Cli.Json where

import           Ambiata.Cli.Json

import           Disorder.Aeson

import           P

import           System.IO

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck

prop_json_temp :: TemporaryAccess -> Property
prop_json_temp = jsonProp


return []
tests :: IO Bool
tests = $quickCheckAll
