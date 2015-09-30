{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Ambiata.Cli.Downloads where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Downloads

import           P

import           Test.QuickCheck

import           System.Directory
import           System.FilePath            ((</>))
import           System.IO

import           Control.Monad.IO.Class     (liftIO)

import           Data.String
import           Data.Text                  (unpack)

import           Mismi.S3 hiding ((</>))

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.Mismi.Amazonka


prop_download :: String -> ServerFile -> Property
prop_download junk f@(ServerFile name) = withLocalAWS $ \p a -> do
  let local = p </> (unpack name)
  liftIO $ writeFile local junk
  uploadOrFail local $ withKey (`combineKey` Key name) a
  fs <- serverFiles a
  pure $ fs === [f]

prop_to_download :: String -> ServerFile -> Property
prop_to_download junk f@(ServerFile name) = withLocalAWS $ \p a -> do
  let dir = DownloadDir p
  let local = p </> (unpack name)
  liftIO $ writeFile local junk
  uploadOrFail local $ withKey (`combineKey` Key name) a
  ls <- filesToDownload dir a
  liftIO $ removeFile local
  ls' <- filesToDownload dir a
  pure $ (ls, ls') === ([], [f])

prop_download_files :: String -> ServerFile -> Property
prop_download_files junk sf@(ServerFile name) = withLocalAWS $ \dir' address -> do
  let dir = DownloadDir dir'

  -- add a file to remote store
  let local = dir' </> (unpack name)
  liftIO $ writeFile local junk
  uploadOrFail local $ withKey (`combineKey` Key name) address

  -- should be nothing locally
  liftIO $ removeFile local
  fileGone <- isFileMissing dir sf

  -- fetch whatever is from the remote address
  res <- downloadFiles dir address
  c <- liftIO $ readFile local

  pure $ (res, c, fileGone) === (DownloadResult [LocalFile name], junk, True)


prop_download_nothing :: ServerFile -> Property
prop_download_nothing (ServerFile name) = withLocalAWS $ \dir' address -> do
  let dir = DownloadDir dir'
  let address' = withKey (`combineKey` Key name) address
  res <- downloadFiles dir address'
  pure $ res === (DownloadResult [])



return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
