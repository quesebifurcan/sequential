{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Protolude

import Sequential4

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Text.Show.Pretty as PP

import Test.QuickCheck

-- generate (arbitrary :: Gen PitchRatio)
-- sample genLimitedRatio
-- generate (arbitrary :: Gen Sound)

genPosRatio :: Gen (Ratio Integer)
genPosRatio = do
  n <- choose (1, 100)
  d <- choose (1, 100)
  return (n % d)

genLimitedRatio :: Gen (Ratio Integer)
genLimitedRatio = fmap limitRatio genPosRatio

instance Arbitrary PitchRatio
  where arbitrary = do
          r <- genLimitedRatio
          return $ PitchRatio r

instance Arbitrary MomentConstraint
  where arbitrary = do
          size'           <- choose (1, 5)
          dissonanceLimit <- vectorOf size' (arbitrary :: Gen PitchRatio)
          maxCount        <- choose (1, 10)
          return $ MomentConstraint (Set.fromList dissonanceLimit) maxCount

instance Arbitrary Pitch
  where arbitrary = do
          pitchRatio <- genLimitedRatio
          octave     <- choose (1, 10)
          return $ Pitch (PitchRatio pitchRatio) (Octave octave)

-- instance Arbitrary MergeStrategy
--   where arbitrary = do
--           return $ MergeStrategy (\sound moment -> Set.insert sound moment)

instance Arbitrary Sound
  where arbitrary = do
          pitch            <- arbitrary :: Gen Pitch
          velocity         <- genLimitedRatio
          start            <- genPosRatio
          -- TODO: `stop` should be greater than `start` + `minDuration`
          stop             <- genPosRatio
          minDuration      <- genPosRatio
          maxDuration      <- genPosRatio
          deltaDuration    <- genPosRatio
          envelope         <- elements [Impulse, Sustained]
          constraint       <- arbitrary :: Gen MomentConstraint
          instrument       <- elements ["a", "b", "c", "d"]
          -- Create generator for phrases as well, using Pending/Resolved
          resolutionStatus <- elements [Resolved]
          verticalGroup    <- arbitrary :: Gen Int
          horizontalGroup  <- arbitrary :: Gen Int
          return $ Sound {
            sound__pitch             = pitch
            , sound__velocity        = Velocity velocity
            , sound__start           = TimePoint start
            , sound__stop            = TimePoint stop
            , sound__minDuration     = Duration minDuration
            , sound__maxDuration     = Duration maxDuration
            , sound__deltaDuration   = Duration deltaDuration
            , sound__envelope        = envelope
            , sound__instrument      = Instrument instrument
            , sound__horizontalGroup = horizontalGroup
            , sound__verticalGroup   = verticalGroup
            , sound__status          = resolutionStatus
            , sound__constraint      = constraint
            }

-- prop_limitRatio pitchRatios =
--   forAll genPosRatio (property . isValid . limitRatio)
--   where isValid x = x >= 1 && x < 2

-- prop_harmonicDistance x =
--   forAll genLimitedRatio
--   where a b =

-- prop_harmonicDistance
-- prop_getGroups3 :: PitchRatio -> Property
-- prop_getGroups3 pitchRatio =
--   pitchRatio === PitchRatio (3 % 4)
--   -- (sum (fmap (length . Map.elems) $ getGroups sounds)) ===
--   -- (length (Map.elems sounds))

printSampleSound = fmap PP.ppDoc (generate (arbitrary :: Gen Sound))
printSampleSounds = fmap PP.ppDoc (sample' (arbitrary :: Gen Sound))

return []
runTests = $quickCheckAll

main = do
  runTests
