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

import Test.QuickCheck

-- generate (arbitrary :: Gen PitchRatio)
-- sample genLimitedRatio
-- generate (arbitrary :: Gen Sound)

limitRatio n
  | n < 1     = limitRatio (n * 2)
  | n >= 2    = limitRatio (n / 2)
  | otherwise = n

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

instance Arbitrary Sound
  where arbitrary = do
          pitch            <- arbitrary :: Gen Pitch
          velocity         <- genLimitedRatio
          start            <- genPosRatio
          stop             <- genPosRatio
          minDuration      <- genPosRatio
          maxDuration      <- genPosRatio
          deltaDuration    <- genPosRatio
          envelope         <- elements [Impulse, Sustained]
          momentConstraint <- arbitrary :: Gen MomentConstraint
          instrument       <- elements ["a", "b", "c", "d"]
          return $ Sound {
            sound__pitch           = pitch
            , sound__velocity      = Velocity velocity
            , sound__start         = TimePoint start
            , sound__stop          = TimePoint stop
            , sound__minDuration   = Duration minDuration
            , sound__maxDuration   = Duration maxDuration
            , sound__deltaDuration = Duration deltaDuration
            , sound__envelope      = envelope
            , sound__constraint    = momentConstraint
            , sound__instrument    = Instrument instrument
            }

-- instance Arbitrary Sound
--   where arbitrary = do
--           pitch <- arbitrary :: Gen Pitch
--           velocity <-
--           return $ Pitch (PitchRatio pitchRatio) (Octave octave)
--   where arbitrary = elements [Open6, Close6]

-- prop_getGroups3 :: Map Int Sound6 -> Property

-- prop_getGroups3 :: PitchRatio -> Property
-- prop_getGroups3 pitchRatio =
--   pitchRatio === PitchRatio (3 % 4)
--   -- (sum (fmap (length . Map.elems) $ getGroups sounds)) ===
--   -- (length (Map.elems sounds))

return []
runTests = $quickCheckAll

main = do
  runTests
