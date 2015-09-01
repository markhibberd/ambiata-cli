{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.TatooineCli.Incoming where

import           TatooineCli.Data
import           TatooineCli.Incoming

import           P                          hiding (isPrefixOf)

import           System.IO

import           Data.Text

import           Test.QuickCheck            hiding ((.&.))
import           Test.TatooineCli.Arbitrary ()


prop_hash_file :: IncomingDir -> IncomingFile -> Bool
prop_hash_file d f =
  ("/" <> (unIncomingFile f)) `isSuffixOf` (pack $ hashFileOf d f)


return []
tests :: IO Bool
tests = $quickCheckAll
