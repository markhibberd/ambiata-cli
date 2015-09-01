{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}


import           Options.Applicative

import           P

import           System.IO
import           System.Log.Logger
import           System.Log.Handler    (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Formatter
import           TatooineCli           (uploadCommand)
import           TatooineCli.Env       (uploadEnv)

data Command = Upload
  deriving (Eq, Show)

cmd :: Parser Command
cmd = subparser (command "upload" (info (pure Upload) ( fullDesc <> progDesc "Upload a dataset to Ambiata.")))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  logh <- streamHandler stdout DEBUG >>= \h ->
            pure $ setFormatter h logFmt
  updateGlobalLogger rootLoggerName (setHandlers [logh])

  execParser (info (helper <*> cmd) (fullDesc <> mainDesc <> header "ambiata - Ambiata CLI")) >>= \case
    Upload -> uploadEnv >>= uploadCommand
  where mainDesc = progDesc "Run `ambiata COMMAND -h` for command-specific help."

        logFmt = simpleLogFormatter "$utcTime - $prio - $msg"
