{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Ambiata.Cli.Http where

import           Ambiata.Cli.Http

import           Control.Concurrent
import           Control.Exception
import           Control.Retry

import           Data.Default

import           Disorder.Core.IO

import           Network
import           Network.HTTP.Client
import           Network.HTTP.Types

import           P

import           System.IO
import           System.Random (randomRIO)

import           Test.QuickCheck

import           Web.Scotty hiding (request)


prop_httpGo :: Property
prop_httpGo =
  forAll (choose (100, 499)) $ \s -> testIO $
  withServer (get "/" . status $ Status s "") $ \req -> do
    mgr <- newManager defaultManagerSettings
    (resp, ec) <- httpGo httpRetryPolicy mgr req
    pure $ (responseStatus resp, ec) === (Status s "", ErrorCount 0)

prop_httpGo_errors :: Property
prop_httpGo_errors =
  forAll (choose (500, 599)) $ \s -> testIO $
  withServer (get "/" . status $ Status s "") $ \req -> do
    mgr <- newManager defaultManagerSettings
    (resp, ec) <- httpGo (limitRetries 5) mgr req
    pure $ (responseStatus resp, ec) === (Status s "", ErrorCount 6)


withServer :: ScottyM () -> (Request -> IO a) -> IO a
withServer app f = do
  port' <- randomRIO (10100, 65534)
  -- Check that we can connect first to avoid flakey "connection refused"
  let connect' = bracket (connectTo "localhost" $ PortNumber (fromInteger $ toInteger port')) hClose pure
  bracket
    (forkIO . scotty port' $ app)
    killThread
    (const $ (recoverAll (limitRetries 5) (const connect')) >> f (def { host = "localhost", port = port' }))


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
