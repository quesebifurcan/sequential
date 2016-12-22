{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sequential where

import Protolude

-- TODO: remove abbreviations
import qualified Data.Either.Validation as V
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Control.Arrow as Arrow
import qualified Data.DList as DL
import qualified System.IO as IO_
import qualified Text.PrettyPrint.Leijen.Text as PP

import System.Random

import qualified Test.QuickCheck as QC

newtype PitchRatio = PitchRatio (Ratio Integer)
  deriving (Enum, Eq, Ord, Num, Show)

newtype Octave = Octave Integer deriving (Eq, Ord, Show)

data Pitch = Pitch {
  ratio      :: PitchRatio,
  octave     :: Octave
  } deriving (Eq, Ord, Show)

octaveEquivalentRatio r
  | r < 1     = octaveEquivalentRatio (r * 2)
  | r >= 2    = octaveEquivalentRatio (r / 2)
  | otherwise = r

isValidPitchRatio (PitchRatio ratio) =
  numerator ratio >= 0 && denominator ratio >= 1

isValidPitch (Pitch ratio (Octave octave)) =
  isValidPitchRatio ratio

newtype Velocity = Velocity (Ratio Integer) deriving (Num, Eq, Ord, Show)

newtype Duration = Duration (Ratio Integer) deriving (Num, Eq, Ord, Show)

-- newtype TimePoint = TimePoint (Ratio Integer) deriving (Num, Eq, Ord, Show)
newtype TimePoint = TimePoint { timePoint :: Ratio Integer } deriving (Num, Eq, Ord, Show)

newtype DissonanceScore = DissonanceScore (Ratio Integer) deriving (Num, Eq, Ord, Show)

data Envelope = Impulse | Sustained deriving (Ord, Eq, Show)

data Instrument = Instrument {
  range :: (Int, Int)
  } deriving (Ord, Eq, Show)

data Sound = Sound {
  pitch           :: Pitch
  , velocity      :: Velocity
  , start         :: TimePoint
  , stop          :: TimePoint
  , minDuration   :: Duration
  , maxDuration   :: Duration
  , deltaDuration :: Duration
  , envelope      :: Envelope
  , constraint    :: MomentConstraint
  , instrument    :: Instrument
  } deriving (Ord, Eq, Show)

data MomentConstraint = MomentConstraint {
  dissonanceLimit :: Set PitchRatio
  , maxCount      :: Int
  } deriving (Ord, Eq, Show)

newtype Instruments = Instruments (Map Text Instrument) deriving Show

data SoundErrors =
  PitchRatioInvalid PitchRatio
  | VelocityRangeError Velocity
  | OctaveRangeError Octave
  | DurationRangeError Duration
  | NoInstrumentError Text
  deriving (Ord, Eq, Show)

instance A.FromJSON Instrument where
   parseJSON (A.Object v) =
     Instrument <$>
     v A..: "range"
   parseJSON invalid = AesonTypes.typeMismatch "Instrument" invalid

instance A.FromJSON Instruments where
    parseJSON val = Instruments <$> A.parseJSON val

mkPitchRatio :: Ratio Integer -> V.Validation [SoundErrors] PitchRatio
mkPitchRatio ratio = bool
  (V.Failure [PitchRatioInvalid ratio'])
  (V.Success ratio')
  (ratio >= 1 && ratio < 2)
  where ratio' = PitchRatio ratio

mkOctave :: Integer -> V.Validation [SoundErrors] Octave
mkOctave octave =
  bool
  (V.Failure [OctaveRangeError octave'])
  (V.Success octave')
  (octave >= 0 && octave <= 10)
  where octave' = Octave octave

mkPitch :: Ratio Integer -> Integer -> V.Validation [SoundErrors] Pitch
mkPitch ratio octave =
  Pitch <$>
  mkPitchRatio ratio <*>
  mkOctave octave

mkVelocity :: Ratio Integer -> V.Validation [SoundErrors] Velocity
mkVelocity velocity =
  bool
  (V.Failure [VelocityRangeError velocity'])
  (V.Success velocity')
  (velocity >= 0 && velocity <= 127)
  where velocity' = (Velocity velocity)

mkDuration :: Ratio Integer -> V.Validation [SoundErrors] Duration
mkDuration duration =
  bool
  (V.Failure [DurationRangeError duration'])
  (V.Success duration')
  (duration > 0)
  where duration' = (Duration duration)

mkInstrument ::
  Instruments -> Text -> V.Validation [SoundErrors] Instrument
mkInstrument (Instruments m) k =
  case result of
    Just v -> V.Success v
    Nothing -> V.Failure [NoInstrumentError k]
  where result = Map.lookup k m

mkSound ::
  Instruments
  -> Ratio Integer
  -> Integer
  -> Ratio Integer
  -> Text
  -> V.Validation [SoundErrors] Sound
mkSound instrumentMap pitchRatio octave velocity instrumentName =
  let pitch' = mkPitch pitchRatio octave
      velocity' = mkVelocity velocity
      instrument' = mkInstrument instrumentMap instrumentName
  in Sound <$>
    pitch' <*>
    velocity' <*>
    pure (TimePoint 0) <*>
    pure (TimePoint 1) <*>
    pure (Duration 1) <*>
    pure (Duration 1) <*>
    pure (Duration 1) <*>
    pure Sustained <*>
    pure MomentConstraint { dissonanceLimit = Set.empty, maxCount = 4 } <*>
    instrument'

data Moment = Moment {
  _now :: TimePoint
  , _active :: Set Sound
  , _result :: DL.DList Sound
  } deriving (Ord, Eq, Show)

momentDefault = Moment {
  _now = TimePoint 0
  , _active = Set.empty
  , _result = DL.empty
  }

-- instrumentMapTest :: Instruments
-- instrumentMapTest =
--   Instruments (Map.fromList [("instrument_1", Instrument { range = (0, 60) })])

minDurationFilled :: TimePoint -> Sound -> Bool
minDurationFilled now sound =
  (start' + minDuration') - now' <= 0
  where (TimePoint start', Duration minDuration') = (start sound, minDuration sound)
        (TimePoint now') = now

maxDurationExceeded :: TimePoint -> Sound -> Bool
maxDurationExceeded now sound =
  now' - start' >= maxDuration'
  where (TimePoint start', Duration maxDuration') = (start sound, maxDuration sound)
        (TimePoint now') = now

distinct :: Ord a => [a] -> [a]
distinct = (Set.toList . Set.fromList)

getPitchRatios :: Sounds -> Set PitchRatio
getPitchRatios = Set.map (ratio . pitch)

getDissonanceScore :: Set PitchRatio -> DissonanceScore
getDissonanceScore pitchRatios =
  if count == 0
     then DissonanceScore (0 % 1)
     else DissonanceScore (sum' % (toInteger count))
  where
    count = length intervals
    complexity ratio = numerator ratio + denominator ratio
    getInterval (PitchRatio x, PitchRatio y) = max x y / min x y
    intervals = distinct (pairs pitchRatios)
    sum' = sum $
      map (complexity . getInterval) $
      intervals

pairs :: (Foldable t1, Ord t) => t1 t -> [(t, t)]
pairs set =
  [(x,y) | let list = toList set, x <- list, y <- list, x < y]

eitherRemoveOne ::
  Ord a =>
  (a -> Bool)
  -> ([a] -> [a])
  -> Set a
  -> Either (Set a) (Set a)
eitherRemoveOne partitionBy sortBy_ xs
  | Set.size a == 0 = Left b
  | otherwise       = Right (Set.union a' b)
  where (a, b) = Set.partition partitionBy xs
        a' = Set.fromList $ (drop 1 . sortBy_) $ Set.toList a

type DissonanceLimit = Set PitchRatio
type Sounds = Set Sound

eitherRemoveSound' :: Moment -> Either Moment Moment
eitherRemoveSound' moment@(Moment now active result)
  | active == Set.empty = Right moment
  | otherwise = case removed of
      Nothing    -> Left moment
      Just sound -> Right $ Moment
          now
          (Set.delete sound active)
          (DL.snoc result (sound { stop = now }))
  where removed =
          head
          . (sortBy (comparing start))
          . Set.toList
          . fst
          . Set.partition (minDurationFilled now)
          $ active

filterMoment :: (Sound -> Bool) -> Moment -> Moment
filterMoment pred (Moment now active result) =
  Moment
  now
  (Set.difference active removed)
  (DL.concat [result, (fmap setStop (DL.fromList (Set.toList removed)))])
  where removed   = Set.filter pred active
        setStop x = x { stop = now }

filterMaxDurationExceeded :: Moment -> Moment
filterMaxDurationExceeded moment@(Moment now _ _) =
  filterMoment (maxDurationExceeded now) moment

applyDecay' :: Moment -> Moment
applyDecay' moment@(Moment now active result) =
  case (eitherRemoveSound' moment) of
    Left _       -> applyDecay' (moment { _now = nextTimePoint })
    Right result -> result
  where nextTimePoint =
          case (getNextSilence active) of
            Nothing -> now
            Just x -> x

soundId :: Sound -> (TimePoint, Pitch, Instrument)
soundId x = (start x, pitch x, instrument x)

addSound' :: Sound -> Moment -> Moment
addSound' sound moment@(Moment now active result) =
  if canAdd
     then Moment now merged result
     else addSound' sound (applyDecay' moment)
  where merged = Set.insert (sound { start = now }) active
        canAdd = Set.size merged == ((+ 1) . Set.size . (Set.map soundId)) active

reduceCount' :: Int -> Moment -> Moment
reduceCount' limit moment@(Moment now active result) =
  bool (reduceCount' limit (applyDecay' moment)) moment test
  where test = (Set.size active <= limit)

reduceDissonance_ :: Set PitchRatio -> Moment -> Moment
reduceDissonance_ limit moment@(Moment now active result) =
  bool (reduceDissonance_ limit (applyDecay' moment)) moment test
  where limit'          = getDissonanceScore limit
        dissonanceScore = getDissonanceScore . Set.map (ratio . pitch)
        test            = (dissonanceScore active) <= limit'

forwardTime :: Duration -> Moment -> Moment
forwardTime inc moment@(Moment now _ _) =
  moment { _now = getTimePoint now inc }

buildConstraint :: Sound -> Moment -> Moment
buildConstraint sound =
  forwardTime (deltaDuration sound)
  . reduceDissonance_ dissonanceLimit'
  . reduceCount' maxCount'
  . addSound' sound
  where dissonanceLimit' = (dissonanceLimit . constraint) sound
        maxCount'        = (maxCount . constraint) sound

resolveMoment :: Moment -> Sound -> Moment
resolveMoment moment sound = resolveConstraint moment
  where resolveConstraint = buildConstraint sound

fadeOut :: Moment -> Moment
fadeOut moment@(Moment now active result) =
  bool (fadeOut $ applyDecay' moment) moment (active == Set.empty)

getTimePoint :: TimePoint -> Duration -> TimePoint
getTimePoint now duration =
  let (TimePoint now')     = now
      (Duration duration') = duration
  in TimePoint (now' + duration')

getNextSilence' :: Sound -> TimePoint
getNextSilence' sound =
  let (TimePoint start')      = start sound
      (Duration minDuration') = minDuration sound
  in TimePoint (start' + minDuration')

getNextSilence = head . sort . Set.toList . (Set.map getNextSilence')

run' :: Foldable t => t Sound -> Moment
run' xs =
  fadeOut result
  where result = foldl' resolveMoment momentDefault xs

-- TODO:
-- 1. change Instrument type to use (midiNote, baseFreq, pitchRatio, Octave)
-- 2. render simple melody

-- printSound sound =
--   show (instrument sound)

-- printSounds sounds = undefined

-- TODO: session?
-- TODO: tempo
-- TODO: sc score file in the following format (skip start/stop)
-- [0.1, [\s_new, \helpscore, 1000, 0, 0, \freq, 440]],
-- [ [ 0.0, [ 's_new', 'sine', 1000, 0, 0, 'frequency', 400 ] ], [ 0.5, [ 's_new', 'sine', 1001, 0, 0, 'frequency', 200 ] ], [ 0.8, [ 's_new', 'sine', 1002, 0, 0, 'frequency', 300 ] ], [ 1, [ 'c_set', 0, 0 ] ] ]

-- TODO: multi-segment sounds. Use a `resolve` or `next` field on the
-- sound itself. If any sound S in attempting to resolve a Moment
-- breaks a not-yet-unfolded multi-segment, postpone the resolution
-- of that sound (apply decay until the multi-segment has reached
-- a stage where it no longer conflicts with S.
--
-- Example of Phrase/Chord:
-- data Sound = Sound { _next :: Maybe Sound } deriving Show

-- TODO: use slopes for melodies?
-- Scale -> subset -> slope "template" -> ordering
