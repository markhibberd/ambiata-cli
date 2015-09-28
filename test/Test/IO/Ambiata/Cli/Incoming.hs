{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Ambiata.Cli.Incoming where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Incoming
import           Ambiata.Cli.Processing     (moveToArchive)

import           P

import           Control.Monad.IO.Class

import           Data.String
import           Data.Text                  (isSuffixOf, unpack)
import           Data.Time.Clock


import           Disorder.Core
import           Disorder.Core.IO

import           System.Directory
import           System.FilePath            ((</>))
import           System.IO
import           System.IO.Temp
import           System.Posix.Files

import           Test.Ambiata.Cli.Arbitrary
import           Test.IO.Ambiata.Cli.Util
import           Test.QuickCheck

prop_listfiles :: String -> UniquePair IncomingFile -> Property
prop_listfiles junk (UniquePair f1 f2) = testIO . withSystemTempDirectory "prop_listfiles" $ \dir -> do
  let fp1 = toFilePath (IncomingDir dir) f1
  let fp2 = toFilePath (IncomingDir dir) f2
  createDirectory $ dir </> ".ignorethis"
  createDirectory $ dir </> ".processing"
  createDirectory $ dir </> "archive"
  writeFile fp1 junk
  createSymbolicLink fp1 fp2
  testAmbiata $ do
    ls <- scanIncoming $ IncomingDir dir
    pure $ (rights ls, lefts ls) === ([f1], [Irregular fp2])

prop_listweirdfiles :: IncomingFile -> Property
prop_listweirdfiles f = testIO . withSystemTempDirectory "prop_listweirdfiles" $ \dir -> do
  let fp = toFilePath (IncomingDir dir) f
  mk <- generate irregular
  mk fp
  testAmbiata $ do
    ls <- scanIncoming $ IncomingDir dir
    pure $ (rights ls, lefts ls) === ([], [Irregular fp])
  -- FIXME(sio): work out a sane way to test devices
  where irregular :: Gen (FilePath -> IO ())
        irregular = elements $
          [ createDirectory
          , flip createNamedPipe 666
          ]

prop_listempty :: Property
prop_listempty = testIO . withSystemTempDirectory "prop_listempty" $ \dir -> testAmbiata $ do
  ls <- scanIncoming $ IncomingDir dir
  pure $ ls === []

prop_hashcalc :: UniquePair String -> Property
prop_hashcalc (UniquePair content content') = testIO . withSystemTempDirectory "prop_hashcalc" $ \dir -> do
  writeFile (toFilePath (IncomingDir dir) (f "1")) content
  writeFile (toFilePath (IncomingDir dir) (f "2")) content
  writeFile (toFilePath (IncomingDir dir) (f "3")) content'
  h1 <- hashOfFile (IncomingDir dir) (f "1")
  h2 <- hashOfFile (IncomingDir dir) (f "2")
  h3 <- hashOfFile (IncomingDir dir) (f "3")
  pure $ (h1 === h2) .&&. ((h3 == h1) === False)
  where
    f = IncomingFile


prop_process_file :: IncomingFile -> String -> Property
prop_process_file f junk = testIO . withSystemTempDirectory "prop_process_file" $ \dir' -> do
  let dir = IncomingDir dir'
  testAmbiata $ do
    prepareDir dir
    liftIO $ writeFile (toFilePath dir f) junk
    now <- liftIO getCurrentTime

    -- no hash - so this will cause one to be created but take no other action
    pFile <- liftIO $ processFile dir f $ NoChangeAfter (addUTCTime 10000 now)

    ls <- scanIncoming dir

    hashExists <- liftIO $ doesFileExist $ (toWorkingPath dir Hashfiles) </> (unpack $ unIncomingFile f)

    -- this will actually move the file into processed
    pFile' <- liftIO $ processFile dir f $ NoChangeAfter (addUTCTime 10000 now)

    ls' <- scanIncoming dir
    hashExists' <- liftIO $ doesFileExist $ (toWorkingPath dir Hashfiles) </> (unpack $ unIncomingFile f)
    inProcessing <- liftIO $ existsInProcessing dir pFile'
    pure $ (ls, ls', inProcessing, hashExists, hashExists', pFile) === ([Right f], [], True, True, False, Nothing)

existsInProcessing :: IncomingDir -> Maybe ProcessingFile -> IO Bool
existsInProcessing _ Nothing = pure False
existsInProcessing dir (Just (ProcessingFile name)) =
        doesFileExist $ (toWorkingPath dir Processing) </> unpack name >>= pure

prop_process_changed_file :: IncomingFile -> String -> Property
prop_process_changed_file f junk = testIO . withSystemTempDirectory "prop_process_changed_file" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  prepareDir dir
  liftIO $ writeFile (toFilePath dir f) junk
  now <- liftIO getCurrentTime
  let longTimeAgo = addUTCTime (-10000) now

  -- no hash - so this will cause one to be created but take no other action
  pFile <- liftIO $ processFile dir f $ NoChangeAfter longTimeAgo
  ls <- scanIncoming dir

  -- this will do nothing as file has changed since longTimeAgo
  pFile' <- liftIO $ processFile dir f $ NoChangeAfter longTimeAgo
  ls' <- scanIncoming dir

  -- but hash will exist
  hashExists <- liftIO $ doesFileExist $ (toWorkingPath dir Hashfiles) </> (unpack $ unIncomingFile f)
  inProcessing <- liftIO $ existsInProcessing dir pFile'
  pure $ (ls, ls', inProcessing, hashExists, pFile) === ([Right f], [Right f], False, True, Nothing)

prop_process_dir :: UniquePair IncomingFile -> String -> Property
prop_process_dir (UniquePair f1 f2) junk = testIO . withSystemTempDirectory "prop_process_dir" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  prepareDir dir
  liftIO $ writeFile (toFilePath dir f1) junk
  liftIO $ writeFile (toFilePath dir f2) junk
  now <- liftIO getCurrentTime

  let future = NoChangeAfter (addUTCTime 10000 now)
  (_, pFiles, bFiles) <- processDir dir future

  -- and again to simulate time passing
  (_, pFiles', bFiles') <- processDir dir future
  ls <- scanIncoming dir

  allInProcessing <- liftIO $ mapM (existsInProcessing dir) (Just <$> pFiles')

  pure $ (ls, length pFiles, length pFiles', allInProcessing, length bFiles, length bFiles') === ([], 0, 2, [True, True], 0, 0)

prop_process_dir_processed :: UniquePair ProcessingFile -> String -> Property
prop_process_dir_processed (UniquePair p1 p2) junk = testIO . withSystemTempDirectory "prop_process_dir_processed" $ \dir -> testAmbiata $ do
  let inc = IncomingDir dir
  let procFile = (toWorkingPath inc Processing </>) . unpack . unProcessingFile
  let (f1, f2) = bimap procFile procFile (p1, p2)
  prepareDir inc
  -- simulate a crash mid-upload
  liftIO $ writeFile f1 junk
  liftIO $ writeFile f2 junk
  future <- liftIO $ (NoChangeAfter . addUTCTime 10000) <$> getCurrentTime
  (iFiles, pFiles, bFiles) <- processDir inc future
  inProcessing <- liftIO $ (length . filter id) <$> (mapM (existsInProcessing inc) (Just <$> pFiles))
  pure $ (length iFiles, length pFiles, inProcessing, length bFiles) === (0, 2, 2, 0)

prop_process_dir_content_change :: UniquePair IncomingFile -> UniquePair String -> Property
prop_process_dir_content_change (UniquePair f1 f2) (UniquePair c1 c2) = testIO
                                                    . withSystemTempDirectory
                                                    "prop_process_dir_content_change" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  prepareDir dir
  liftIO $ writeFile (toFilePath dir f1) c1
  liftIO $ writeFile (toFilePath dir f2) c1
  now <- liftIO getCurrentTime

  let future = NoChangeAfter (addUTCTime 10000 now)
  (_, pFiles, bFiles) <- processDir dir future

  -- write a change to f2
  liftIO $ writeFile (toFilePath dir f2) c2

  -- Like the sands of time...
  (lsPrev, pFiles', bFiles') <- processDir dir future
  ls <- scanIncoming dir

  -- Although time has passed, f2 changed, so only f1 should have made it. f2 stays in IncomingDir
  let processedNames = fmap (isSuffixOf $ unIncomingFile f1) (unProcessingFile <$> pFiles')

  pure $ (ls, length pFiles, length pFiles', processedNames, length lsPrev, length bFiles, length bFiles') === ([Right f2], 0, 1, [True], 2, 0, 0)

prop_cleanup_archived :: Retention -> ProcessingFile -> String -> Property
prop_cleanup_archived r f1 c1 = testIO . withCurrentTime $ \now ->
  withSystemTempDirectory "prop_process_dir_content_change" $ \dir' -> testAmbiata $ do
    future <- liftIO . generate $ futureWithin now r
    farFuture <- liftIO . generate $ futureOutside now r
    let dir = IncomingDir dir'
    prepareDir dir
    liftIO $ writeFile (toWorkingPath dir Processing </> unpack (unProcessingFile f1)) c1
    a1 <- liftIO $ moveToArchive dir f1
    arc1 <- cleanUpArchived dir (Retention 1) future
    ls1 <- filter visible <$> (liftIO $ getDirectoryContents (toWorkingPath dir Archive))
    arc2 <- cleanUpArchived dir (Retention 1) farFuture
    ls2 <- filter visible <$> (liftIO $ getDirectoryContents (toWorkingPath dir Archive))
    pure $ (arc1, arc2, ls1, ls2) === ([], [a1], [unpack $ unProcessingFile f1], [])
    where withCurrentTime f = liftIO getCurrentTime >>= f

-- file error cases.
-- need to check for case if moving a file and it isn't there.
-- also need to check for if go to delete a hashfile that is already deleted

prop_file_removed_err :: IncomingFile -> String -> Property
prop_file_removed_err f junk = testIO . withSystemTempDirectory "prop_file_removed_err" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  prepareDir dir
  liftIO $ writeFile (toFilePath dir f) junk
  now <- liftIO getCurrentTime

  -- no hash - so this will cause one to be created but take no other action
  _ <- liftIO $ processFile dir f $ NoChangeAfter (addUTCTime 10000 now)
  ls <- scanIncoming dir

  -- do something naughty
  liftIO $ removeFile (toFilePath dir f)

  _ <- liftIO $ processFile dir f $ NoChangeAfter (addUTCTime 10000 now)
  ls' <- scanIncoming dir

  pure $ ls === [Right f] .&&. ls' === []


prop_cleanup_junk :: UniquePair IncomingFile -> String -> Property
prop_cleanup_junk (UniquePair f1 f2) junk = testIO . withSystemTempDirectory "prop_cleanup_junk" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  prepareDir dir
  let hashDir = toWorkingPath dir Hashfiles
  now <- liftIO getCurrentTime

  -- file 1 will be legit
  liftIO $ writeFile (toFilePath dir f1) junk
  liftIO $ writeFile (toFilePath dir f2) junk

  -- this creates the hash directory and files
  _ <- processDir dir $ NoChangeAfter now

  -- get rid of a content file
  liftIO $ removeFile (toFilePath dir f2)

  -- but hashes still exist
  hashExists1 <- liftIO $ doesFileExist $ hashDir </> (unpack $ unIncomingFile f1)
  hashExists2 <- liftIO $ doesFileExist $ hashDir </> (unpack $ unIncomingFile f2)

  liftIO $ cleanUpHashfiles dir

  hashExists1' <- liftIO $ doesFileExist $ hashDir </> (unpack $ unIncomingFile f1)
  hashExists2' <- liftIO $ doesFileExist $ hashDir </> (unpack $ unIncomingFile f2)

  pure $ hashExists1 .&&. hashExists2 .&&. hashExists1' .&&. hashExists2' === False

prop_prepare_dir :: Property
prop_prepare_dir = testIO . withSystemTempDirectory "prop_prepare_dir" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  archive <- liftIO $ doesDirectoryExist $ toWorkingPath dir Archive
  hashfiles <- liftIO $ doesDirectoryExist $ toWorkingPath dir Hashfiles
  processing <- liftIO $ doesDirectoryExist $ toWorkingPath dir Processing

  prepareDir dir

  archive' <- liftIO $ doesDirectoryExist $ toWorkingPath dir Archive
  hashfiles' <- liftIO $ doesDirectoryExist $ toWorkingPath dir Hashfiles
  processing' <- liftIO $ doesDirectoryExist $ toWorkingPath dir Processing

  let newincoming = dir' </> "new"
  newdir <- liftIO $ doesDirectoryExist newincoming

  prepareDir $ IncomingDir newincoming
  newdir' <- liftIO $ doesDirectoryExist newincoming

  pure $ conjoin [newdir === False,
                  archive === False,
                  hashfiles === False,
                  processing === False,
                  archive' === True,
                  hashfiles' === True,
                  processing' === True,
                  newdir' === True]


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
