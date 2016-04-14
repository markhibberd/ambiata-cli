{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Ambiata.Cli.Data
import           Ambiata.Cli.Standalone

import           BuildInfo_ambiata_cli

import           Control.Concurrent (getNumCapabilities, setNumCapabilities)

import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           GHC.Conc (getNumProcessors)

import qualified Mismi.Amazonka as A

import           Options.Applicative

import           P

import           System.IO
import           System.Environment
import           System.Exit
import           X.Options.Applicative
import           X.Control.Monad.Trans.Either.Exit (orDie)


data Command =
    UploadCommand FilePath
  | UploadExecCommand BufferSize FileName Program Arguments
    deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  x <- getNumCapabilities
  case x of
    1 -> do
      numProcs <- getNumProcessors
      setNumCapabilities numProcs
    _ ->
      pure ()

  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

parser :: Parser (SafeCommand Command)
parser =
  safeCommand . subparser . mconcat $ [
      command' "upload" "Upload a single file" $
        UploadCommand <$> fileP
    , command' "upload-exec" "Execute a program and upload its standard output." $
        UploadExecCommand <$> bufferP <*> fileNameP <*> progP <*> argsP
    ]

run :: Command -> IO ()
run c = case c of
  UploadCommand f -> do
    k <- AmbiataAPIKey <$> text "AMBIATA_API_KEY"
    a <- AmbiataAPIEndpoint <$> textOr "AMBIATA_API_ENDPOINT" "https://api.ambiata.com"
    orDie renderUploadError $
      upload A.Sydney k a f
  UploadExecCommand b f p args -> do
    k <- AmbiataAPIKey <$> text "AMBIATA_API_KEY"
    a <- AmbiataAPIEndpoint <$> textOr "AMBIATA_API_ENDPOINT" "https://api.ambiata.com"
    orDie renderUploadError $
      uploadExec A.Sydney k a f p args b

fileP :: Parser FilePath
fileP =
  argument str . mconcat $ [
      metavar "FILE"
    , help "File path to upload."
    ]

fileNameP :: Parser FileName
fileNameP =
  fmap FileName . argument textRead . mconcat $ [
      metavar "FILE_NAME"
    , help "File name to use for upload."
    ]

progP :: Parser Program
progP =
  fmap Program . argument textRead . mconcat $ [
      metavar "PROGRAM"
    , help "Program to execute."
    ]

argsP :: Parser Arguments
argsP =
  fmap Arguments . many . argument textRead . mconcat $ [
      metavar "ARGUMENTS"
    , help "Arguments of program to execute."
    ]

bufferP :: Parser BufferSize
bufferP =
  fmap BufferSize . option auto . mconcat $ [
      long "buffer-size"
    , metavar "BUFFER_SIZE_BYTES"
    , help "Buffer size of upload chunks, a direct control on memory usage vs chunk performance, defaults to 100MB chunks."
    , val $ 1024 * 1024 * 100
    ]

text :: String -> IO Text
text e =
  lookupEnv e >>=
    maybe (bomb . T.pack $ e <> " is a required environment variable for this command.") (pure . T.pack)

textOr :: String -> Text -> IO Text
textOr e v =
  lookupEnv e >>=
    maybe (pure v) (pure . T.pack)

bomb :: Text -> IO a
bomb msg =
  T.hPutStrLn stderr msg >> exitFailure
