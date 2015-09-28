{-# LANGUAGE NoImplicitPrelude #-}
module Test.IO.Ambiata.Cli.Util where

import           Ambiata.Cli.Data

import           P

import           Control.Monad.Trans.Either

import           Disorder.Core

import           Test.QuickCheck


testAmbiata :: (Monad m, Applicative m) => EitherT AmbiataError m Property -> m Property
testAmbiata prop = (runEitherT prop) >>= (pure . either (failWith . renderClientError) id)

