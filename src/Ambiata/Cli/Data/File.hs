{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.File (
    LocalFile (..)
  , FilesystemError (..)
  , renderFilesystemError
  , renderFileList
  ) where

import           Control.Exception (IOException)

import           Data.Text (Text)
import qualified Data.Text as T

import           P


newtype LocalFile = LocalFile {
  unLocalFile :: Text
} deriving (Show, Eq, Ord)

data FilesystemError =
  UnknownFilesystemError IOException deriving (Show, Eq)


renderFilesystemError :: FilesystemError -> Text
renderFilesystemError e =
  case e of
    UnknownFilesystemError t ->
      "Unknown filesystem error: " <> (T.pack $ show t)

renderFileList :: [Text] -> Text
renderFileList [] = "None"
renderFileList fs = T.intercalate ", " fs
