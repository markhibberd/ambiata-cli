{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Ambiata.Cli.Data.Api
import           Ambiata.Cli.Data.Download (unServerFile)
import           Ambiata.Cli.Data.Exec
import           Ambiata.Cli.Data.Env
import           Ambiata.Cli.Data.Transfer
import           Ambiata.Cli.Standalone

import           BuildInfo_ambiata_cli

import           Control.Concurrent (getNumCapabilities, setNumCapabilities)

import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           GHC.Conc (getNumProcessors)

import qualified Mismi.S3 as S3

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
  | ListCommand Organisation Endpoint
  | DownloadCommand Organisation Endpoint S3.Address FilePath
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
    , command' "download-listing" "List files available for download" $
        ListCommand <$> orgP <*> endP
    , command' "download" "Download specified file" $
        DownloadCommand <$> orgP <*> endP <*> sourceP <*> targetP
    ]

run :: Command -> IO ()
run c = case c of
  UploadCommand f -> do
    k <- AmbiataAPIKey <$> text "AMBIATA_API_KEY"
    r <- textParseOr "AMBIATA_REGION" AmbiataAu parseAmbiataRegion
    a <- AmbiataAPIEndpoint <$> textOr "AMBIATA_API_ENDPOINT" (unAmbEndpoint . defaultAmbiataAPIEndpointFor $ r)
    orDie renderUploadError $
      upload (toAWSRegion r) k a f
  UploadExecCommand b f p args -> do
    k <- AmbiataAPIKey <$> text "AMBIATA_API_KEY"
    r <- textParseOr "AMBIATA_REGION" AmbiataAu parseAmbiataRegion
    a <- AmbiataAPIEndpoint <$> textOr "AMBIATA_API_ENDPOINT" (unAmbEndpoint . defaultAmbiataAPIEndpointFor $ r)
    orDie renderUploadError $
      uploadExec (toAWSRegion r) k a f p args b
  ListCommand o e -> do
    k <- AmbiataAPIKey <$> text "AMBIATA_API_KEY"
    r <- textParseOr "AMBIATA_REGION" AmbiataAu parseAmbiataRegion
    a <- AmbiataAPIEndpoint <$> textOr "AMBIATA_API_ENDPOINT" (unAmbEndpoint . defaultAmbiataAPIEndpointFor $ r)
    fs <- orDie renderListError $
      list k a o e
    forM_ fs $ \f ->
      T.putStrLn . S3.addressToText . unServerFile $ f
  DownloadCommand o e s t -> do
    k <- AmbiataAPIKey <$> text "AMBIATA_API_KEY"
    r <- textParseOr "AMBIATA_REGION" AmbiataAu parseAmbiataRegion
    a <- AmbiataAPIEndpoint <$> textOr "AMBIATA_API_ENDPOINT" (unAmbEndpoint . defaultAmbiataAPIEndpointFor $ r)
    orDie renderDownloadError $
      download (toAWSRegion r) k a o e s t

fileP :: Parser FilePath
fileP =
  argument str . mconcat $ [
      metavar "FILE"
    , help "File path to upload."
    ]

targetP :: Parser FilePath
targetP =
  argument str . mconcat $ [
      metavar "FILE"
    , help "File path to download to"
    ]

sourceP :: Parser S3.Address
sourceP =
  argument (pOption S3.s3Parser) . mconcat $ [
      metavar "S3_ADDRESS"
    , help "S3 address to request for download."
    ]

fileNameP :: Parser FileName
fileNameP =
  fmap FileName . argument textRead . mconcat $ [
      metavar "FILE_NAME"
    , help "File name to use for upload."
    ]

orgP :: Parser Organisation
orgP =
  fmap Organisation . argument textRead . mconcat $ [
      metavar "ORGANISATION_ID"
    , help "Your organisation id, e.g. 12345679."
    ]

endP :: Parser Endpoint
endP =
  fmap Endpoint . argument textRead . mconcat $ [
      metavar "ENDPOINT_ID"
    , help "Your endpoint id, e.g. 12345679."
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
    , value $ 1024 * 1024 * 100
    ]

text :: String -> IO Text
text e =
  lookupEnv e >>=
    maybe (bomb . T.pack $ e <> " is a required environment variable for this command.") (pure . T.pack)

textOr :: String -> Text -> IO Text
textOr e v =
  lookupEnv e >>=
    maybe (pure v) (pure . T.pack)

textParseOr :: String -> a -> (Text -> Maybe a) -> IO a
textParseOr e v p =
  lookupEnv e >>=
    maybe (pure v) (fromMaybeM (bomb . T.pack $ e <> " could not be parsed.") . p . T.pack)

bomb :: Text -> IO a
bomb msg =
  T.hPutStrLn stderr msg >> exitFailure
