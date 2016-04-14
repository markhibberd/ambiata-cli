{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Ambiata.Cli.Processing where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Incoming
import           Ambiata.Cli.Processing

import           P

import           Test.QuickCheck

import           System.Directory
import           System.FilePath            ((</>))
import           System.IO
import           System.IO.Temp

import           Control.Monad.IO.Class     (liftIO)

import           Data.List                  (nub, sort)
import           Data.String
import           Data.Text                  (unpack)

import           Disorder.Core.IO

import           Mismi
import           Mismi.S3

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.IO.Ambiata.Cli.Util

import qualified Test.Mismi.S3 as S3
import           Test.Mismi.S3

import           X.Control.Monad.Trans.Either


prop_available_files :: String -> [ProcessingFile] -> Property
prop_available_files junk files' = testIO . withSystemTempDirectory "prop_available_files" $ \dir' -> testAmbiata $ do
  let xfiles = nub files'
  let dir = IncomingDir dir'
  let working = toWorkingPath dir Processing
  prepareDir dir
  ls <- liftIO $ availableFiles dir
  liftIO $ mapM_ (\p -> writeFile (working </> (unpack $ unProcessingFile p)) $ junk) xfiles
  ls' <- liftIO $ availableFiles dir
  pure $ (ls, length ls', sort ls') === ([], length xfiles, sort (xfiles))

prop_move_to_archive :: String -> ProcessingFile -> Property
prop_move_to_archive junk f@(ProcessingFile name) = testIO . withSystemTempDirectory "prop_move_to_archive" $ \dir' -> testAmbiata $ do
  let dir = IncomingDir dir'
  let working = toWorkingPath dir Processing
  prepareDir dir
  liftIO $ writeFile (working </> (unpack $ name)) junk
  (ArchivedFile archivedName) <- liftIO $ moveToArchive dir f
  ls <- liftIO $ getDirectoryContents (toWorkingPath dir Archive)
  pure $ (elem (unpack name) ls, name) === (True, archivedName)


prop_upload :: String -> ProcessingFile -> Property
prop_upload junk f@(ProcessingFile name) = withLocalAWS $ \p a -> do
  let dir = IncomingDir p
  void $ liftIO $ runEitherT $ prepareDir dir
  let working = toWorkingPath dir Processing
  liftIO $ writeFile (working </> (unpack $ name)) junk
  archivedFiles <- processReady dir a
  ls <- liftIO $ availableFiles dir
  let d = (p </> unpack name)
  downloadOrFail (fileAddress f a) d
  c <- liftIO $ readFile d
  pure $ (length archivedFiles, length ls, c) === (1,0,junk)

-- check it leaves files in processing
prop_upload_broken :: String -> ProcessingFile -> TemporaryAccess -> Property
prop_upload_broken junk (ProcessingFile name) creds' = testIO . withSystemTempDirectory "prop_upload_broken" $ \dir' -> runOrFail $ do
  let dir = IncomingDir dir'
  let working = toWorkingPath dir Processing
  creds <- liftIO $ useTestBucket creds'
  void $ liftIO $ runEitherT $ prepareDir dir
  liftIO $ writeFile (working </> (unpack $ name)) junk
  ls <- liftIO $ availableFiles dir
  -- this should actually fail
  _ <- expectLeft "Not valid creds" $ uploadReady dir Sydney (UploadAccess creds)
  -- and leave the file in the processing dir:
  ls' <- liftIO $ availableFiles dir
  pure $ (ls, length ls) ===  (ls', 1)



--
-- | some neat combinators to make tests a bit nicer:
--
withLocalAWS :: Testable a => (FilePath -> Address -> AWS a) -> Property
withLocalAWS x = S3.testAWS $
  join $ x <$> S3.newFilePath <*> S3.newAddress

runOrFail :: (Monad m) => EitherT Text m a -> m a
runOrFail = (=<<) (either (fail . unpack) return) . runEitherT

expectLeft :: Functor m => f -> EitherT b m e -> EitherT f m b
expectLeft err e =  bimapEitherT (const err) id $ swapEitherT e

useTestBucket :: TemporaryAccess -> IO TemporaryAccess
useTestBucket (TemporaryAccess creds (Address _ k)) = do
  b' <- testBucket
  return $ TemporaryAccess creds (Address b' k)

swapEitherT :: Functor m => EitherT a m b -> EitherT b m a
swapEitherT =
  newEitherT . fmap (either Right Left) . runEitherT



return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
