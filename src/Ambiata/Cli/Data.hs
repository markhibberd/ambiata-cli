{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ambiata.Cli.Data where

import           Ambiata.Cli.Json

import           Control.Exception

import           P

import           System.FilePath
import           System.Locale

import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Time.Clock
import           Data.Time.Format

import           Mismi

import           Network.HTTP.Types     (Header, hAuthorization)


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

newtype AmbiataAPIKey = AmbiataAPIKey {
    unAmbKey :: Text
  } deriving (Eq, Show)

newtype AmbiataAPIEndpoint = AmbiataAPIEndpoint {
    unAmbEndpoint :: Text
  } deriving (Show)

newtype ServerFile = ServerFile {
  unServerFile :: Text
} deriving (Show, Eq, Ord)

newtype DownloadDir = DownloadDir {
  unDownloadDir :: FilePath
} deriving (Show, Eq, Ord)

newtype LocalFile = LocalFile {
  unLocalFile :: Text
} deriving (Show, Eq, Ord)

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

renderFileList :: [Text] -> Text
renderFileList [] = "None"
renderFileList fs = T.intercalate ", " fs

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

newtype DownloadResult = DownloadResult { unDownloadResult :: [LocalFile] }
  deriving (Show, Eq, Ord)

renderDownloadResult :: DownloadResult -> Text
renderDownloadResult (DownloadResult fs) =
  "Downloaded files: " <> renderFileList (unLocalFile <$> fs)

newtype DownloadAccess = DownloadAccess { unDownloadAccess :: TemporaryAccess }
  deriving (Eq, Show)

newtype UploadAccess = UploadAccess { unUploadAccess :: TemporaryAccess }
  deriving (Eq, Show)

data RunMode = Daemon | OneShot
  deriving (Eq, Show)

data Verbosity = Verbose | Quiet
  deriving (Eq, Show)

data CommonEnv = CommonEnv {
    apiKey      :: AmbiataAPIKey
  , apiEndpoint :: AmbiataAPIEndpoint
  , runMode     :: RunMode
  , verbosity   :: Verbosity
} deriving (Show)

newtype Retention = Retention { retentionDays :: Int }
  deriving (Eq, Show, Ord)

renderRetention :: Retention -> Text
renderRetention (Retention 0) = "never"
renderRetention (Retention a) = T.pack (show a) <> " days"

data UploadEnv = UploadEnv IncomingDir Retention CommonEnv
  deriving (Show)

data DownloadEnv = DownloadEnv DownloadDir CommonEnv
  deriving (Show)

type EnvErrors = Text

data CredentialLoadError = BadResponse Text | DecodeError Text
  deriving (Show, Eq)

data FilesystemError = UnknownFilesystemError IOException
  deriving (Show, Eq)

renderFilesystemError :: FilesystemError -> Text
renderFilesystemError (UnknownFilesystemError t) = "Unknown filesystem error: " <> (T.pack $ show t)

renderCredentialError :: CredentialLoadError -> Text
renderCredentialError (BadResponse t) = "An error occurred with the Ambiata API: " <> t
renderCredentialError (DecodeError t) = "Incorrect JSON returned from Ambiata API: " <> t

data AmbiataError =
    AmbiataCredentialLoadError CredentialLoadError
  | AmbiataAWSError Error
  | AmbiataEnvError EnvErrors
  | AmbiataFilesystemError FilesystemError

renderClientError :: AmbiataError -> Text
renderClientError (AmbiataCredentialLoadError e) = renderCredentialError e
renderClientError (AmbiataAWSError a) = errorRender a
renderClientError (AmbiataEnvError t) = t
renderClientError (AmbiataFilesystemError e) = renderFilesystemError e


instance Show NoChangeAfter where
  show f = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" $ unTime f

processingPrefix :: UTCTime -> String
processingPrefix = formatTime defaultTimeLocale "%H-%M-%S-"

data FileReady =
    Ready
  | NotHashed
  | HashNotOldEnough
  | HashChanged LatestHash
  deriving (Show, Eq, Ord)

toFilePath :: IncomingDir -> IncomingFile -> FilePath
toFilePath (IncomingDir d) (IncomingFile f) = d </> (T.unpack f)

toWorkingPath :: IncomingDir -> WorkingDir -> FilePath
toWorkingPath (IncomingDir d) =
                        \case
                            Archive -> d </> "archive"
                            Processing -> d </> ".processing"
                            Hashfiles -> d </> ".hashfiles"

tokenHeader :: AmbiataAPIKey -> Header
tokenHeader (AmbiataAPIKey k) = (hAuthorization, encodeUtf8 $ "token " <> k)
