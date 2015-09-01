{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.TatooineCli.Json where

import           TatooineCli.Json
import           Test.TatooineCli.Arbitrary ()

import           Disorder.Aeson

import           P

import           System.IO

import           Test.QuickCheck

prop_json_temp :: TemporaryAccess -> Property
prop_json_temp = jsonProp


return []
tests :: IO Bool
tests = $quickCheckAll
