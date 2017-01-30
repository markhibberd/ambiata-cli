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
  | AmbiataSg
  | AmbiataAuPii
  | AmbiataUsPii
  | AmbiataSgPii
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
    AmbiataSg ->
      AmbiataAPIEndpoint "https://singapore-api.ambiata.com"
    AmbiataAuPii ->
      AmbiataAPIEndpoint "https://pii-api.ambiata.com"
    AmbiataUsPii ->
      AmbiataAPIEndpoint "https://pii-us-api.ambiata.com"
    AmbiataSgPii ->
      AmbiataAPIEndpoint "https://pii-singapore-api.ambiata.com"

renderAmbiataRegion :: AmbiataRegion -> Text
renderAmbiataRegion r =
  case r of
    AmbiataAu ->
      "au"
    AmbiataUs ->
      "us"
    AmbiataSg ->
      "sg"
    AmbiataAuPii ->
      "pii-au"
    AmbiataUsPii ->
      "pii-us"
    AmbiataSgPii ->
      "pii-sg"

parseAmbiataRegion :: Text -> Maybe AmbiataRegion
parseAmbiataRegion t =
  case t of
    "au" ->
      Just AmbiataAu
    "us" ->
      Just AmbiataUs
    "sg" ->
      Just AmbiataSg
    "pii-au" ->
      Just AmbiataAuPii
    "pii-us" ->
      Just AmbiataUsPii
    "pii-sg" ->
      Just AmbiataSgPii
    _ ->
      Nothing

toAWSRegion :: AmbiataRegion -> Region
toAWSRegion r =
  case r of
    AmbiataAu ->
      Sydney
    AmbiataUs ->
      NorthVirginia
    AmbiataSg ->
      Singapore
    AmbiataAuPii ->
      Sydney
    AmbiataUsPii ->
      NorthVirginia
    AmbiataSgPii ->
      Singapore

awsRegion :: CommonEnv -> Region
awsRegion c =
  toAWSRegion $ apiRegion c

apiEndpoint :: CommonEnv -> AmbiataAPIEndpoint
apiEndpoint c =
  fromMaybe (defaultAmbiataAPIEndpointFor $ apiRegion c) $ explicitApiEndpoint c
