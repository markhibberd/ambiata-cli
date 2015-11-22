{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Disorder.Core.Main

import qualified Test.Cli.Ambiata.Cli.Linking

main :: IO ()
main =
  disorderMain $ fmap ($ exe) [
    Test.Cli.Ambiata.Cli.Linking.tests
  ]
  where
    exe = "./dist/build/ambiata-daemon/ambiata-daemon"
