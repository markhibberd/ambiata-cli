{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Ambiata.Cli.Linking (
    tests
  ) where

import qualified Data.List as L
import qualified Data.Text as T

import           Disorder.Core.Property ((=\\=))
import           Disorder.Core.IO

import           P

import           System.IO              hiding (FilePath)

import           Test.Ambiata.Cli

import           Test.QuickCheck
import           Test.QuickCheck.Test

import           Turtle

-- Ensure we don't accidentally add any object dependencies, and that
-- libgmp isn't here.
prop_object_dependencies :: Text -> Property
prop_object_dependencies exe = testIO $ do
  (st, out) <- testShell' [ "objdump", "-p", exe ]
  let
    soname (_:n:[]) = Just n
    soname _        = Nothing
  
    deplines = filter (T.isInfixOf "NEEDED") $ T.lines out
    deps = (filter (not . T.null) . T.words) <$> deplines
    sonames = L.sort . catMaybes $ soname <$> deps
    
  pure $ conjoin [
      st === ExitSuccess
    , sonames =\\= expected
    ]
  where
    expected = [
        "libz.so.1"
      , "librt.so.1"
      , "libutil.so.1"
      , "libdl.so.2"
      , "libm.so.6"
      , "libpthread.so.0"
      , "libc.so.6"
      ]


tests :: Text -> IO Bool
tests exe = fmap (all isSuccess) $
  mapM (quickCheckWithResult stdArgs { maxSuccess = 1 }) $ fmap ($ exe) [
      prop_object_dependencies
    ]
