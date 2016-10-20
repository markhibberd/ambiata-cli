{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ambiata.Cli.Incoming (
    processDir
  , scanIncoming
  , hashOfFile
  , processFile
  , hashFileOf
  , cleanUpArchived
  , cleanUpHashfiles
  , prepareDir
  , visible
) where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Files

import           Control.Exception
import           Control.Monad.IO.Class     (liftIO)

import           Crypto.Hash.MD5            (hashlazy)

import qualified Data.ByteString            as Strict
import qualified Data.ByteString.Lazy       as Lazy
import           Data.Either
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Foreign.C.Types            (CTime (..))

import           P

import           System.Directory (getDirectoryContents, doesFileExist, removeFile)
import           System.Directory (createDirectoryIfMissing, getModificationTime, renameFile)
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Posix.Files

import           Text.Printf

import           X.Control.Monad.Trans.Either

--
-- INCOMING FOR HANDLING FILES COMING INTO AMBIATA
-- /dropbox, /dropbox/.processing, /dropbox/.hashfiles, /dropbox/archive
--

hoistIOError :: Either IOException a -> EitherT AmbiataError IO a
hoistIOError = either (left . AmbiataFilesystemError . UnknownFilesystemError) right

-- |
-- Scan for files that have had no "recent" changes, and move them
-- to processing dir if ready. Hashfiles may be created. Tears and laughter.
--
processDir :: IncomingDir -> NoChangeAfter -> EitherT AmbiataError IO ([IncomingFile], [ProcessingFile], [BadFileState])
processDir dir age = do
  (badIncoming, incomingFiles) <- partitionEithers <$> scanIncoming dir
  (badOld, old) <- partitionEithers <$> scanProcessing dir -- find files left over from previous upload attempts
  fs <- liftIO $ mapM (flip (processFile dir) age) incomingFiles
  liftIO $ cleanUpHashfiles dir
  right $ (incomingFiles, catMaybes fs <> old, badIncoming <> badOld)

-- |
-- prepare the incoming dir with working directories.
--
prepareDir :: IncomingDir -> EitherT AmbiataError IO ()
prepareDir dir = hoistIOError =<< liftIO (try prepareDir')
  where prepareDir' = do
          mkdir $ unDir dir
          mkdir $ toWorkingPath dir Processing
          mkdir $ toWorkingPath dir Hashfiles
          mkdir $ toWorkingPath dir Archive

        mkdir = createDirectoryIfMissing True

-- |
-- Look for leftover files in .processing (from crashes during upload, etc.).
scanProcessing :: IncomingDir -> EitherT AmbiataError IO [Either BadFileState ProcessingFile]
scanProcessing d = do
  fs <- scanDir wd
  pure $ (bimap id ProcessingFile) <$> fs
  where wd = toWorkingPath d Processing

-- |
-- Look for candidate files in the current directory only. Non recursive.
-- Don't change anything.
scanIncoming :: IncomingDir -> EitherT AmbiataError IO [Either BadFileState IncomingFile]
scanIncoming d = do
  fs <- scanDir $ unDir d
  pure $ (second IncomingFile) <$> fs

scanDir :: FilePath -> EitherT AmbiataError IO [Either BadFileState Text]
scanDir dir = do
  candidates <- liftIO $ filter ignores <$> (getDirectoryContents $ dir)
  fs <- mapM checkGoodness candidates
  right fs
  where
      ignores f = case f of
        ('.':_) -> False
        "archive" -> False -- Don't generate a warning for archive.
        (_:_) -> True
        _ -> False

      checkGoodness :: FilePath -> EitherT AmbiataError IO (Either BadFileState Text)
      checkGoodness fp = getFileState (dir </> fp) >>= \case
        GoodFile        -> pure . Right $ T.pack fp
        BadFile badness -> pure $ Left badness

getFileState :: FilePath -> EitherT AmbiataError IO FileState
getFileState f = do
  t <- liftIO $ catch ((Right . typeFromStat) <$> getSymbolicLinkStatus f) handleIOError
  either (left . AmbiataFilesystemError) right t
  where typeFromStat :: FileStatus -> FileState
        typeFromStat st
          | isRegularFile st     = GoodFile
          | isDirectory st       = BadFile (Irregular f)
          | isSymbolicLink st    = BadFile (Irregular f)
          | isSocket st          = BadFile (Irregular f)
          | isNamedPipe st       = BadFile (Irregular f)
          | isCharacterDevice st = BadFile (Irregular f)
          | isBlockDevice st     = BadFile (Irregular f)
          | otherwise            = BadFile (FileStateUnknown f)

        handleIOError :: IOError -> IO (Either FilesystemError FileState)
        handleIOError e
          | isPermissionError e   = pure . Right $ BadFile (StatPermissionDenied f)
          | isDoesNotExistError e = pure . Right $ BadFile (Disappeared f)
            -- The above two should cover all the error cases, so catch fire
            -- if we get here.
          | otherwise             = pure . Left $ UnknownFilesystemError e

-- |
-- Files that have stable hashes and are old enough.
-- these are the files are are ready to go.
isFileReady :: IncomingDir -> IncomingFile -> NoChangeAfter -> IO FileReady
isFileReady dir f (NoChangeAfter noChangeAfter) = do
  let hashFile = hashFileOf dir f
  hashed <- doesFileExist hashFile
  source <- doesFileExist $ toFilePath dir f
  if hashed && source
    then do
      lastMod <- getModificationTime hashFile
      if (lastMod < noChangeAfter)
        then do
          previous <- recordedHashOfFile dir f
          latest <- hashOfFile dir f
          pure $ if hashIsConsistent previous latest then Ready else HashChanged latest
        else
          pure HashNotOldEnough
      else
        pure NotHashed

moveToProcessing :: IncomingDir -> IncomingFile -> IO ProcessingFile
moveToProcessing dir f = do
  ignoreNoFile $ removeFile $ hashFileOf dir f
  let filename = unIncomingFile f
  ignoreNoFile $ renameFile (toFilePath dir f) $ (toWorkingPath dir Processing)
                                                 </>
                                                 T.unpack filename
  pure $ ProcessingFile filename

updateHashFile :: IncomingDir -> IncomingFile -> LatestHash ->  IO ()
updateHashFile dir f (LatestHash hash) = do
  TIO.writeFile (hashFileOf dir f) hash

processFile :: IncomingDir -> IncomingFile -> NoChangeAfter -> IO (Maybe ProcessingFile)
processFile dir f age =
  isFileReady dir f age >>= \fileState -> case fileState of
    Ready ->
      Just <$> moveToProcessing dir f
    NotHashed ->
      ignoreNoFile (hashOfFile dir f >>= updateHashFile dir f) >> pure Nothing
    HashNotOldEnough ->
      pure Nothing
    HashChanged h ->
      Nothing <$ updateHashFile dir f h

hashFileOf :: IncomingDir -> IncomingFile -> FilePath
hashFileOf dir f =
  (toWorkingPath dir Hashfiles) </> (T.unpack $ unIncomingFile f)

hashIsConsistent  :: RecordedHash -> LatestHash -> Bool
hashIsConsistent r l =
  unRecordedHash r == unHash l

recordedHashOfFile :: IncomingDir -> IncomingFile -> IO RecordedHash
recordedHashOfFile d f =
  fmap RecordedHash . TIO.readFile $ hashFileOf d f

hashOfFile :: IncomingDir -> IncomingFile -> IO LatestHash
hashOfFile d f = do
  bytes <- fmap hashlazy . Lazy.readFile $ toFilePath d f
  pure $ LatestHash $ T.pack $ Strict.unpack bytes >>= printf "%02x"


cleanUpArchived :: IncomingDir -> Retention -> UTCTime -> EitherT AmbiataError IO ([ArchivedFile])
cleanUpArchived inc retention utcNow = do
  candidates <- (fmap (adir </>) . filter visible) <$> (liftIO $ getDirectoryContents adir)
  deletes <- filterM deletable candidates
  mapM_ cleanup deletes
  pure ((ArchivedFile . T.pack . takeFileName) <$> deletes)
  where adir = toWorkingPath inc Archive

        deletable fp = do
          st <- hoistIOError =<< (liftIO $ try (getFileStatus fp))
          pure $ oldEnough (statusChangeTime st)

        oldEnough (CTime ct) = posixSecondsToUTCTime (fromIntegral ct) < daysAgo utcNow retention

        cleanup fp = hoistIOError =<< (liftIO $ try (removeLink fp))

visible :: FilePath -> Bool
visible ('.':_) = False
visible (_:_)   = True
visible _       = False



daysAgo :: UTCTime -> Retention -> UTCTime
daysAgo now (Retention d) = addUTCTime epochDays now
  where epochDays = (- (posixDayLength * (fromIntegral d)))

-- |
-- could accumulate little hashfiles due to crashes,
-- so why not clean them up if they have no content file.
cleanUpHashfiles :: IncomingDir -> IO ()
cleanUpHashfiles dir = do
  let d = (toWorkingPath dir Hashfiles)
  hashfiles <- (getDirectoryContents d) >>= filterM (doesFileExist . (</>) d)
  mapM_ (maybeRemoveHashfile dir) $ (IncomingFile . T.pack) <$> hashfiles


maybeRemoveHashfile :: IncomingDir -> IncomingFile -> IO Bool
maybeRemoveHashfile dir f = do
  ifM (doesFileExist $ toFilePath dir f)
      (pure False)
      (do
        ignoreNoFile $ removeFile (hashFileOf dir f)
        pure True)
