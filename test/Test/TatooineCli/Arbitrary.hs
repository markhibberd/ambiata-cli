{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.TatooineCli.Arbitrary where

import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           TatooineCli.Data
import           TatooineCli.Json


import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import           Disorder.Corpus

import           Test.Mismi.Arbitrary      ()
import           Test.Mismi.S3.Arbitrary   ()

import           Data.Text

posixDays :: Int -> NominalDiffTime
posixDays = (posixDayLength *) . fromIntegral

futureWithin :: UTCTime -> Retention -> Gen UTCTime
futureWithin now (Retention d) = (flip addUTCTime now) <$> arbitrary `suchThat` closeEnough
  where closeEnough t =
          (t > (fromIntegral (0 :: Int))) && (t < posixDays d)

futureOutside :: UTCTime -> Retention -> Gen UTCTime
futureOutside now (Retention d) = (flip addUTCTime now) <$> arbitrary `suchThat` farEnough
  where farEnough t =
          (t > (fromIntegral (0 :: Int))) && (t > posixDays d)

instance Arbitrary Retention where
  arbitrary = Retention <$> (arbitrary `suchThat` (> 1))

instance Arbitrary IncomingDir where
  arbitrary = (IncomingDir . unpack) <$> elements cooking

instance Arbitrary IncomingFile where
  arbitrary = IncomingFile <$> elements muppets

instance Arbitrary ProcessingFile where
  arbitrary = ProcessingFile <$> elements southpark

instance Arbitrary WorkingDir where
  arbitrary = elements [Archive, Processing, Hashfiles]

instance Arbitrary TemporaryAccess where
  arbitrary = TemporaryAccess <$> arbitrary
                               <*> arbitrary

instance Arbitrary TemporaryCreds where
  arbitrary = TemporaryCreds <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AmbiataAPIKey where
  arbitrary = AmbiataAPIKey <$> elements muppets

instance Arbitrary ServerFile where
  arbitrary = ServerFile <$> elements cooking
