{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sequential4 where

import Protolude
import GHC.Show

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.DList as DL
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Error.Class as Error
import Control.Monad.Trans.Either

newtype PitchRatio = PitchRatio (Ratio Integer)
  deriving (Enum, Eq, Ord, Num, Show)

newtype Octave = Octave Integer
  deriving (Eq, Ord, Show)

data Pitch = Pitch {
  pitch__ratio  :: PitchRatio,
  pitch__octave :: Octave
  } deriving (Eq, Ord, Show)

newtype Velocity = Velocity (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

newtype Duration = Duration (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

newtype TimePoint = TimePoint (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

newtype DissonanceScore = DissonanceScore (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

data Envelope = Impulse | Sustained
  deriving (Ord, Eq, Show)

newtype Instrument = Instrument Text
  deriving (Eq, Ord, Show)

data Sound = Sound {
  sound__pitch           :: Pitch
  , sound__velocity      :: Velocity
  , sound__start         :: TimePoint
  , sound__stop          :: TimePoint
  , sound__minDuration   :: Duration
  , sound__maxDuration   :: Duration
  , sound__deltaDuration :: Duration
  , sound__envelope      :: Envelope
  , sound__constraint    :: MomentConstraint
  , sound__instrument    :: Instrument
  } deriving (Ord, Eq, Show)

data MomentConstraint = MomentConstraint {
  momentConstraint__dissonanceLimit :: Set PitchRatio
  , momentConstraint__maxCount      :: Int
  } deriving (Ord, Eq, Show)

newtype Instruments = Instruments (Map Text Instrument) deriving Show

data Moment = Moment {
  moment__now      :: TimePoint
  , moment__active :: Set Sound
  , moment__result :: [Sound]
  } deriving (Ord, Eq, Show)

limitRatio :: (Ord a, Fractional a) => a -> a
limitRatio n
  | n < 1     = limitRatio (n * 2)
  | n >= 2    = limitRatio (n / 2)
  | otherwise = n

harmonicDistance :: Floating a => Ratio Integer -> a
harmonicDistance r =
  logBase 2 . fromIntegral $ n * d
  where r' = limitRatio r
        n  = numerator r'
        d  = denominator r'

distinct :: Ord a => [a] -> [a]
distinct = (Set.toList . Set.fromList)

pairs :: (Foldable t1, Ord t) => t1 t -> [(t, t)]
pairs set =
  [(x,y) | let xs = toList set, x <- xs, y <- xs, x < y]

limitedInterval (PitchRatio x, PitchRatio y) =
  max limX limY / min limX limY
  where limX = limitRatio x
        limY = limitRatio y

dissonanceScore ::
  (Foldable t1, Floating t) => t1 PitchRatio -> t
dissonanceScore pitchRatios =
  if count == 0
    then 0
    else sum' / (fromIntegral count)
  where
    count     = length intervals
    intervals = distinct (pairs pitchRatios)
    sum'      = sum $
      fmap (harmonicDistance . limitedInterval) intervals

isDissonant = undefined

insertSound = undefined

anyRemovable = undefined

anyResolved = undefined

allResolved = undefined

isPending = undefined

maxDurationExceeded = undefined

minDurationFilled = undefined

applyDecay = undefined

forwardTime = undefined

reduceDissonance = undefined

reduceCount = undefined

getNextMinDuration = undefined

isMember = undefined

getGroups = undefined
