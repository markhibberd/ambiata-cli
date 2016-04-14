{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Upload (
    module X
  , IncomingDir (..)
  , ProcessingFile (..)
  , IncomingFile (..)
  , ArchivedFile (..)
  , LatestHash (..)
  , RecordedHash (..)
  , WorkingDir (..)
  , NoChangeAfter (..)
  , FileState (..)
  , BadFileState (..)
  , UploadResult (..)
  , UploadAccess (..)
  , Retention (..)
  , UploadEnv (..)
  , FileReady (..)
  , TemporaryAccess (..)
  , renderBadFileState
  , renderUploadResult
  , toFilePath
  , toWorkingPath
  , renderRetention
  ) where

import           Ambiata.Cli.Data.Env
import           Ambiata.Cli.Data.File
import           Ambiata.Cli.Data.Transfer as X

import qualified Data.Text as T
import           Data.Time (UTCTime, formatTime)

import           Mismi.S3 (Address)

import           P

import           System.FilePath ((</>))
import           System.IO (FilePath)
import           System.Locale (defaultTimeLocale)


newtype IncomingDir = IncomingDir {
      unDir :: FilePath
  } deriving (Show, Eq, Ord)

newtype ProcessingFile = ProcessingFile {
  unProcessingFile :: Text
  } deriving (Show, Eq, Ord)

newtype IncomingFile = IncomingFile  {
    unIncomingFile :: Text
  } deriving (Show, Eq, Ord)

newtype ArchivedFile = ArchivedFile {
    unArchivedFile :: Text
  } deriving (Show, Eq, Ord)

newtype LatestHash = LatestHash {
  unHash :: Text
  } deriving (Show, Eq, Ord)

newtype RecordedHash = RecordedHash {
  unRecordedHash :: Text
  } deriving (Show, Eq, Ord)

data WorkingDir = Archive | Processing | Hashfiles
    deriving (Show, Eq, Ord)

newtype NoChangeAfter = NoChangeAfter {
    unTime :: UTCTime
  } deriving (Eq, Ord)

-- |
-- Only 'GoodFile's will be uploaded; anything else will cause a warning
-- to be printed.
data FileState =
      -- | Standard file/hard link.
      GoodFile
      -- | Everything else.
    | BadFile BadFileState
  deriving (Show, Eq)

-- |
-- Various ways a file-like thing can be unacceptable for us to upload
-- on a POSIX system.
data BadFileState =
      -- | Directories, symlinks, pipes, sockets and block/char devices.
      Irregular FilePath
      -- | ENOENT - should only happen if deleted or moved after we call
      -- getdirents(2).
    | Disappeared FilePath
      -- | EPERM - should only happen if we lose the execute bit on the
      -- IncomingFiles directory after we call getdirents(2) and before
      -- we call lstat(2).
    | StatPermissionDenied FilePath
      -- | Shouldn't happen, indicates logic bug (or maybe running on a
      -- POSIX-incompatible system).
    | FileStateUnknown FilePath
  deriving (Show, Eq)

renderBadFileState :: BadFileState -> Text
renderBadFileState (Irregular f)            = (T.pack f) <> " - uploads must be regular files (not symlinks or directories)"
renderBadFileState (Disappeared f)          = (T.pack f) <> " - file disappeared - moved or deleted?"
renderBadFileState (StatPermissionDenied f) = (T.pack f) <> " - permission denied - check file and directory permissions"
renderBadFileState (FileStateUnknown f)     = (T.pack f) <> " - unable to determine file status - are you running on a modern Linux system?"

data UploadResult = UploadResult {
    incomingList   :: [IncomingFile]
  , processingList :: [ProcessingFile]
  , archivedList   :: [ArchivedFile]
} deriving (Show, Eq, Ord)

renderUploadResult :: UploadResult -> Text
renderUploadResult (UploadResult is ps as)
  | null is && null ps && null as = "No files to upload."
  | otherwise                     = T.intercalate "\n" $
      [ "Upload processing complete."
      , "Incoming files: "   <> renderFileList (unIncomingFile <$> is)
      , "Ready for upload: " <> renderFileList (unProcessingFile <$> ps)
      , "Uploaded: "         <> renderFileList (unArchivedFile <$> as)
      ]

newtype UploadAccess = UploadAccess { unUploadAccess :: TemporaryAccess }
  deriving (Eq, Show)

newtype Retention =
  Retention {
      retentionDays :: Int
    }
    deriving (Eq, Show, Ord)

data UploadEnv =
  UploadEnv IncomingDir Retention CommonEnv deriving (Show)

data FileReady =
    Ready
  | NotHashed
  | HashNotOldEnough
  | HashChanged LatestHash
  deriving (Show, Eq, Ord)

toFilePath :: IncomingDir -> IncomingFile -> FilePath
toFilePath (IncomingDir d) (IncomingFile f) =
  d </> (T.unpack f)

toWorkingPath :: IncomingDir -> WorkingDir -> FilePath
toWorkingPath (IncomingDir d) =
  \case
    Archive ->
      d </> "archive"
    Processing ->
      d </> ".processing"
    Hashfiles ->
      d </> ".hashfiles"

data TemporaryAccess =
  TemporaryAccess {
    tempCreds :: TemporaryCreds,
    s3Path    :: Address
  } deriving (Eq, Show)

instance Show NoChangeAfter where
  show f = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" $ unTime f

renderRetention :: Retention -> Text
renderRetention r =
  case r of
    Retention 0 ->
      "never"
    Retention a ->
      T.pack (show a) <> " days"
