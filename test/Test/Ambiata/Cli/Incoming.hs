{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Ambiata.Cli.Incoming where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Incoming

import           P                          hiding (isPrefixOf)

import           System.IO

import           Data.Text

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.QuickCheck            hiding ((.&.))


prop_hash_file :: IncomingDir -> IncomingFile -> Bool
prop_hash_file d f =
  ("/" <> (unIncomingFile f)) `isSuffixOf` (pack $ hashFileOf d f)


return []
tests :: IO Bool
tests = $quickCheckAll
