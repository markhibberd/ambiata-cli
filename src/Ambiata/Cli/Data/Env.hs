{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ambiata.Cli.Data.Env (
    RunMode (..)
  , Verbosity (..)
  , CommonEnv (..)
  , AmbiataRegion (..)
  , defaultAmbiataRegion
  , defaultAmbiataAPIEndpoint
  , defaultAmbiataAPIEndpointFor
  , renderAmbiataRegion
  , parseAmbiataRegion
  , toAWSRegion
  , apiEndpoint
  , awsRegion
  ) where

import           Ambiata.Cli.Data.Api

import           Mismi (Region (..))

import           P

data RunMode = Daemon | OneShot
  deriving (Eq, Show)

data Verbosity = Verbose | Quiet
  deriving (Eq, Show)

data CommonEnv =
  CommonEnv {
      envCredential :: AmbiataAPICredential
    , explicitApiEndpoint :: Maybe AmbiataAPIEndpoint
    , apiRegion :: AmbiataRegion
    , runMode :: RunMode
    , verbosity :: Verbosity
    } deriving (Show)


data AmbiataRegion =
    AmbiataAu
  | AmbiataUs
    deriving (Eq, Show)


defaultAmbiataRegion :: AmbiataRegion
defaultAmbiataRegion =
  AmbiataAu

defaultAmbiataAPIEndpoint :: AmbiataAPIEndpoint
defaultAmbiataAPIEndpoint =
  defaultAmbiataAPIEndpointFor defaultAmbiataRegion

defaultAmbiataAPIEndpointFor :: AmbiataRegion -> AmbiataAPIEndpoint
defaultAmbiataAPIEndpointFor r =
  case r of
    AmbiataAu ->
      AmbiataAPIEndpoint "https://api.ambiata.com"
    AmbiataUs ->
      AmbiataAPIEndpoint "https://us-api.ambiata.com"

renderAmbiataRegion :: AmbiataRegion -> Text
renderAmbiataRegion r =
  case r of
    AmbiataAu ->
      "au"
    AmbiataUs ->
      "us"

parseAmbiataRegion :: Text -> Maybe AmbiataRegion
parseAmbiataRegion t =
  case t of
    "au" ->
      Just AmbiataAu
    "us" ->
      Just AmbiataUs
    _ ->
      Nothing

toAWSRegion :: AmbiataRegion -> Region
toAWSRegion r =
  case r of
    AmbiataAu ->
      Sydney
    AmbiataUs ->
      NorthVirginia

awsRegion :: CommonEnv -> Region
awsRegion c =
  toAWSRegion $ apiRegion c

apiEndpoint :: CommonEnv -> AmbiataAPIEndpoint
apiEndpoint c =
  fromMaybe (defaultAmbiataAPIEndpointFor $ apiRegion c) $ explicitApiEndpoint c
