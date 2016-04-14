{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Exec (
    Program (..)
  , Arguments (..)
  , FileName (..)
  , BufferSize (..)
  , procify
  ) where

import qualified Data.Text as T

import           P

import           System.Process (CreateProcess (..), proc)

newtype BufferSize =
  BufferSize {
      bufferSize :: Int
    } deriving (Eq, Show)

newtype Program =
  Program {
      program :: Text
    } deriving (Eq, Show)

newtype Arguments =
  Arguments {
      arguments :: [Text]
    } deriving (Eq, Show)

newtype FileName =
  FileName {
      fileName :: Text
    } deriving (Eq, Show)

procify :: Program -> Arguments -> CreateProcess
procify p args =
  proc (T.unpack . program $ p) (fmap T.unpack . arguments $ args)
