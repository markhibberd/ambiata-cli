{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Ambiata.Cli
import           Ambiata.Cli.Env

import           BuildInfo_ambiata_cli

import           Options.Applicative

import           P

import           System.IO
import           System.Log.Logger
import           System.Log.Handler    (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Formatter
import           System.Exit

import           X.Options.Applicative

data Command =
    Upload
  | Download
  deriving (Eq, Show)

cmd :: Parser Command
cmd =
      subparser (command "upload" (info (pure Upload) ( fullDesc <> progDesc "Upload a dataset to Ambiata.")))
  -- FIX Remove 'internal' when download has been cleaned up
  <|> subparser (command "download" (info (pure Download) ( fullDesc <> progDesc "Download datasets from Ambiata.")))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  logh <- streamHandler stdout DEBUG >>= \h ->
            pure $ setFormatter h logFmt
  updateGlobalLogger rootLoggerName (setHandlers [logh])

  dispatch cli >>= \case
    VersionCommand -> putStrLn ("ambiata-daemon: " <> buildInfoVersion) >> exitSuccess
    RunCommand DryRun c -> print c >> exitSuccess
    RunCommand RealRun c -> run c
  where cli = safeCommand cmd
        logFmt = simpleLogFormatter "$utcTime - $prio - $msg"

run :: Command -> IO ()
run c = do
  case c of
    Upload -> uploadEnv >>= uploadCommand
    Download -> downloadEnv >>= downloadCommand
