{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Ambiata.Cli.Json where

import qualified Ambiata.Cli.Json.Upload as U
import qualified Ambiata.Cli.Json.Download as D

import           Disorder.Aeson

import           P

import           System.IO

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck

prop_upload = jsonProp . U.ResponseJsonV1
prop_download = jsonProp . D.ResponseJsonV1


return []
tests :: IO Bool
tests = $quickCheckAll
