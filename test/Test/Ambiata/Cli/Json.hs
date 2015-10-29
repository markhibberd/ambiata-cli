{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Ambiata.Cli.Json where

import           Ambiata.Cli.Json.Upload

import           Disorder.Aeson

import           P

import           System.IO

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck

prop_json_temp = jsonProp . ResponseJsonV1


return []
tests :: IO Bool
tests = $quickCheckAll
