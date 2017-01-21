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

data ResolutionStatus = Pending | Resolved
  deriving (Eq, Ord, Show)

data Sound = Sound {
  sound__pitch             :: Pitch
  , sound__velocity        :: Velocity
  , sound__instrument      :: Instrument
  , sound__minDuration     :: Duration
  , sound__maxDuration     :: Duration
  , sound__start           :: TimePoint
  , sound__stop            :: TimePoint
  , sound__deltaDuration   :: Duration
  , sound__envelope        :: Envelope
  -- , sound__add          :: Sound -> Moment -> Moment
  -- , sound__subtract     :: Moment -> Moment
  -- , sound__isValid      :: Moment -> Bool
  , sound__horizontalGroup :: Int
  , sound__verticalGroup   :: Int
  , sound__status          :: ResolutionStatus
  , sound__constraint      :: MomentConstraint
  } deriving (Ord, Eq, Show)

-- add, subtract, isValid
-- IFF all functions (add, subtract, isValid etc.) work in the same way,
-- is it possible to achieve all variants?

-- soundId = verticalId (never more than one instance of id x active at any point in time).
-- groupId = horizontalId (no sound in group x is ever removed until at least one sound in active with id x has the status "Resolved")

soundDefault =
  Sound
  { sound__pitch =
    Pitch
    { pitch__ratio = PitchRatio (24 % 13) , pitch__octave = Octave 4 }
  , sound__velocity = Velocity (88 % 45)
  , sound__instrument = Instrument "c"
  , sound__minDuration = Duration (77 % 30)
  , sound__maxDuration = Duration (34 % 21)
  , sound__start = TimePoint (13 % 10)
  , sound__stop = TimePoint (51 % 98)
  , sound__deltaDuration = Duration (51 % 11)
  , sound__envelope = Impulse
  , sound__horizontalGroup = 1
  , sound__verticalGroup = 8
  , sound__status = Resolved
  , sound__constraint =
    MomentConstraint
    { momentConstraint__dissonanceLimit =
      Set.fromList
      [ PitchRatio (17 % 16)
      , PitchRatio (19 % 12)
      , PitchRatio (51 % 32)
      , PitchRatio (120 % 67)
      , PitchRatio (176 % 95)
      ]
    , momentConstraint__maxCount = 10
    }
  }

data MomentConstraint = MomentConstraint {
  momentConstraint__dissonanceLimit :: Set PitchRatio
  , momentConstraint__maxCount      :: Int
  } deriving (Ord, Eq, Show)

newtype Instruments = Instruments (Map Text Instrument) deriving Show

data Moment = Moment {
  moment__now      :: TimePoint
  , moment__active :: Set Sound
  , moment__result :: [Sound]
  }
  deriving (Ord, Eq, Show)

------------------------------------------------------------
-- 1. Use Silence
-- 2. Gestures as do blocks. Actions: up/down, new phrase when out of range,
-- nextDuration, sample etc.
-- spanning several phrases/groups, loop over finite sets of elements

data GestureState = GestureState {
  -- __scale :: [Int]
  -- , durs :: [Duration]
  vels :: [Velocity]
  , groupId :: Int
  } deriving (Show)

-- transpose s@(GestureState scale) = s { __vels = drop 1 scale }
coll (x, xs) = (x, xs ++ [x])

-- getSound curr (GestureState vels) =
--   fmap curr { sound__velocity =

-- run3 :: EitherT ResolutionStatus (State Moment2) ()

type GestureA = State ((Int, Int), [Int]) ()

inc1 = modify f
  where f ((a, b), c) = ((a + 1, b), c)
inc2 = modify f
  where f ((a, b), c) = ((a, b + 1), c)

coll1 = modify f
  where f ((a, b), c) = ((a, b), c ++ [a])
coll2 = modify f
  where f ((a, b), c) = ((a, b), c ++ [b])

gestureA :: GestureA
gestureA = do
  coll1
  inc1
  inc2
  coll2
  inc2
  coll2
  coll1
  inc2
  coll2
  inc2
  coll2
  inc2
  coll2
  return ()

gestureB :: EitherT [Int] (State (Int, [Int], [Int])) ()
gestureB = do
  (a, (b:bs), c) <- get
  modify next
  if a > b
     then modify (\(a, b, c) -> (a, b, c ++ [100]))
     else return ()
  if bs == []
     then (do modify next
              (_, _, c) <- get
              left c)
     else gestureB
  where
    next (a, [], c)     = (a, [], c ++ [a])
    next (a, (b:bs), c) = (b, bs, c ++ [a])

-- Use ranges for the different params (Pitch etc.)
-- When exceeding a range, switch to next group (or do something else)
gestureC :: State (Int, [Int]) ()
gestureC = do
  up (20, 30) 3
  up (20, 30) 3
  up (20, 30) 3
  where
    up :: (Int, Int) -> Int -> State (Int, [Int]) ()
    up (lo, hi) n = do
      replicateM_ n (modify f)
      modify (\(curr, result) -> (curr, result ++ [curr]))
      where
        f (curr, result) =
          case (curr < lo, curr > hi) of
            (True, _) -> (hi, result)
            (_, True) -> (lo, result)
            _         -> (curr + 1, result)

------------------------------------------------------------
-- Functions

limitRatio :: (Ord a, Fractional a) => a -> a
limitRatio n
  | n < 1     = limitRatio (n * 2)
  | n >= 2    = limitRatio (n / 2)
  | otherwise = n

harmonicDistance :: Floating a => Ratio Integer -> a
harmonicDistance r =
  logBase 2 . fromIntegral $ (numerator r * denominator r)

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

isRemovable = undefined

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
