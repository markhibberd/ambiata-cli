{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Ambiata.Cli.Linking (
    tests
  ) where

import qualified Data.List as L
import qualified Data.Text as T

import           Disorder.Core.IO

import           P

import           System.IO              hiding (FilePath)

import           Test.Ambiata.Cli

import           Test.QuickCheck
import           Test.QuickCheck.Test

import           Turtle

-- Ensure libgmp isn't in the built executable's object dependencies.
prop_static_gmp :: Text -> Property
prop_static_gmp exe = testIO $ do
  (st, out) <- testShell' [ "ldd", exe ]
  pure $ (st, "libgmp" `T.isInfixOf` (T.toLower out)) === (ExitSuccess, False)

-- Ensure we don't accidentally add any object dependencies.
prop_object_dependencies :: Text -> Property
prop_object_dependencies exe = testIO $ do
  (st, out) <- testShell' [ "ldd", exe ]
  let n = L.length $ T.lines out
  pure $ (st, n) === (ExitSuccess, 9)

tests :: Text -> IO Bool
tests exe = fmap (all isSuccess) $
  mapM (quickCheckWithResult stdArgs { maxSuccess = 1 }) $ fmap ($ exe) [
      prop_static_gmp
    , prop_object_dependencies
    ]
