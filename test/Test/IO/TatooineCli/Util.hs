{-# LANGUAGE NoImplicitPrelude #-}
module Test.IO.TatooineCli.Util where

import           P

import           Control.Monad.Trans.Either

import           Disorder.Core

import           TatooineCli.Data

import           Test.QuickCheck


testTatooine :: (Monad m, Applicative m) => EitherT TatooineClientError m Property -> m Property
testTatooine prop = (runEitherT prop) >>= (pure . either (failWith . renderClientError) id)

