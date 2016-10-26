{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ambiata.Cli.Arbitrary where

import           Ambiata.Cli.Data

import           Data.Text
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Disorder.Corpus

import           Mismi.S3 (Key (..), combineKey, withKey)

import           P

import           Test.Mismi.Arbitrary      ()
import           Test.Mismi.S3.Arbitrary   ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.TSRP.Arbitrary ()

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

instance Arbitrary DownloadAccess where
  arbitrary =
    DownloadAccess
      <$> arbitrary
      <*> arbitrary

instance Arbitrary TemporaryCreds where
  arbitrary = TemporaryCreds <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AmbiataAPIKey where
  arbitrary = AmbiataAPIKey <$> elements muppets

instance Arbitrary LocalFile where
  arbitrary =
    LocalFile
      <$> elements weather

instance Arbitrary ServerFile where
  arbitrary =
    createServerFileOrFail =<<
      (\a k -> withKey (flip combineKey k) a)
        <$> arbitrary
        <*> (Key . unLocalFile <$> arbitrary)

instance Arbitrary AmbiataRegion where
  arbitrary =
    elements [AmbiataAu, AmbiataUs]

instance Arbitrary AmbiataAPICredential where
  arbitrary =
    TSRPCredential <$> arbitrary <*> arbitrary <*> arbitrary

deriving instance Show AmbiataAPICredential

deriving instance Show CommonEnv
