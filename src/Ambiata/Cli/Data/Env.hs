{-# LANGUAGE NoImplicitPrelude #-}
module Ambiata.Cli.Data.Env (
    RunMode (..)
  , Verbosity (..)
  , CommonEnv (..)
  ) where

import           Ambiata.Cli.Data.Api

import           P


data RunMode = Daemon | OneShot
  deriving (Eq, Show)

data Verbosity = Verbose | Quiet
  deriving (Eq, Show)

data CommonEnv =
  CommonEnv {
      apiKey      :: AmbiataAPIKey
    , apiEndpoint :: AmbiataAPIEndpoint
    , runMode     :: RunMode
    , verbosity   :: Verbosity
    } deriving (Show)
