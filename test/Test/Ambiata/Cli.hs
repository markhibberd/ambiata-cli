{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Ambiata.Cli where

import qualified Data.Text        as T

import           P

import           System.IO

import           Turtle

testShell :: [Text] -> Shell Text -> IO (ExitCode, Text)
testShell args = shellStrict args'
  where
    args' = T.intercalate " " args

testShell' :: [Text] -> IO (ExitCode, Text)
testShell' = flip testShell empty
