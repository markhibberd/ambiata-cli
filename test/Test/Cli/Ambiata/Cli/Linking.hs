{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Ambiata.Cli.Linking where

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

tests :: Text -> IO Bool
tests exe = fmap (all isSuccess) $
  mapM (quickCheckWithResult stdArgs { maxSuccess = 1 }) $ fmap ($ exe) [ prop_static_gmp ]
