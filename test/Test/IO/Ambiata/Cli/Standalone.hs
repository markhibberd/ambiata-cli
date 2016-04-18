{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.IO.Ambiata.Cli.Standalone where

import           Ambiata.Cli.Data
import qualified Ambiata.Cli.Standalone as Standalone

import           P

import           Test.QuickCheck

import           System.FilePath            ((</>))
import           System.IO

import           Control.Monad.Reader

import           Data.Text                  (unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi
import           Mismi.S3

import           Test.Ambiata.Cli.Arbitrary ()
import qualified Test.Mismi.S3 as S3

import           X.Control.Monad.Trans.Either


prop_upload :: Text -> Property
prop_upload txt  = withLocalAWS $ \p a -> do
  env <- ask
  let f = p </> "file"
  liftIO $ T.writeFile f txt
  liftIO . runOrFail . bimapEitherT Standalone.renderUploadError id $ do
    Standalone.upload' env a f
  r <- read a
  pure $ r === Just txt

prop_uploadExec :: Text -> Property
prop_uploadExec txt = (not . T.null $ txt) ==> withLocalAWS $ \p a -> do
  env <- ask
  let f = p </> "file"
  liftIO $ T.writeFile f txt
  liftIO . runOrFail . bimapEitherT Standalone.renderUploadError id $ do
    Standalone.uploadExec' env a (Program "cat") (Arguments [T.pack $ f]) (BufferSize $ 1024 * 1024 * 10)
  r <- read a
  pure $ r === Just txt

prop_uploadExecBigger :: Text -> Property
prop_uploadExecBigger txt = (not . T.null $ txt) ==> withLocalAWS $ \p a -> do
  env <- ask
  let big = T.replicate 100 txt
  let f = p </> "file"
  liftIO $ T.writeFile f big
  liftIO . runOrFail . bimapEitherT Standalone.renderUploadError id $ do
    Standalone.uploadExec' env a (Program "cat") (Arguments [T.pack $ f]) (BufferSize $ 1024 * 1024 * 10)
  r <- read a
  pure $ r === Just big

prop_download :: Text -> Property
prop_download txt  = withLocalAWS $ \p a -> do
  env <- ask
  let f = p </> "file"
  writeOrFail a txt
  liftIO . runOrFail . bimapEitherT Standalone.renderDownloadError id $ do
    Standalone.download' env a f
  r <- liftIO $ T.readFile f
  pure $ r === txt

--
-- | some neat combinators to make tests a bit nicer:
--
withLocalAWS :: Testable a => (FilePath -> Address -> AWS a) -> Property
withLocalAWS x = S3.testAWS $
  join $ x <$> S3.newFilePath <*> S3.newAddress

runOrFail :: (Monad m) => EitherT Text m a -> m a
runOrFail = (=<<) (either (fail . unpack) return) . runEitherT

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
