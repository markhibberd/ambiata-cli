{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Ambiata.Cli.Downloads where

import           Ambiata.Cli.Data
import           Ambiata.Cli.Downloads

import           Data.Map (Map)
import qualified Data.Map as M

import           P

import           Test.QuickCheck

import           System.Directory
import           System.FilePath            ((</>))
import           System.IO

import           Control.Monad.IO.Class     (liftIO)

import           Data.Text (Text)
import qualified Data.Text as T

import           Mismi.S3 hiding ((</>))

import           Test.Ambiata.Cli.Arbitrary ()
import           Test.Mismi.Amazonka


prop_to_download_missing local =
  withDownload (M.singleton local "") $ \dir files -> do
    ls <- downloadFiles dir files
    liftIO . removeFile $ unDownloadDir dir </> (T.unpack . unLocalFile) local
    ls' <- downloadFiles dir files
    pure $ (ls, ls') === (DownloadResult [local], DownloadResult [local])

prop_download_files local =
  withDownload local $ \dir files -> do
    r1 <- downloadFiles dir files
    c1 <- liftIO $ readFiles dir r1
    r2 <- downloadFiles dir files
    c2 <- liftIO $ readFiles dir r2
    pure $ (c1, c2) === (local, M.empty)

prop_download_files_more l1 l2' =
  withLocalAWS $ \dir' address -> do
    let dir = DownloadDir dir'
    let l2 = M.difference l2' l1
    f1 <- writeFiles address l1
    r1 <- downloadFiles dir f1
    c1 <- liftIO $ readFiles dir r1
    f2 <- writeFiles address l2
    r2 <- downloadFiles dir f2
    c2 <- liftIO $ readFiles dir r2
    pure $ (c1, c2) === (l1, l2)


withDownload :: Testable a => Map LocalFile Text -> (DownloadDir -> [ServerFile] -> AWS a) -> Property
withDownload files f =
  withLocalAWS $ \dir' address -> do
    sfiles <- writeFiles address files
    f (DownloadDir dir') sfiles

writeFiles :: Address -> Map LocalFile Text -> AWS [ServerFile]
writeFiles address files =
  for (M.toList files) $ \(LocalFile name, junk) -> do
    let a = withKey (`combineKey` Key name) address
    writeOrFail a junk
    createServerFileOrFail a

readFiles :: DownloadDir -> DownloadResult -> IO (Map LocalFile Text)
readFiles (DownloadDir dir) (DownloadResult res) =
  fmap M.fromList . for res $ \f ->
    fmap ((,) f . T.pack) . readFile . (dir </>) . T.unpack . unLocalFile $ f


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
