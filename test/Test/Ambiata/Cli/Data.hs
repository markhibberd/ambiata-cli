{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Ambiata.Cli.Data where

import           Ambiata.Cli.Data

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


return []
tests :: IO Bool
tests = $quickCheckAll
