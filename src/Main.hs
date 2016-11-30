{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Protolude
import qualified Data.Either.Validation as V
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Control.Arrow as Arrow

newtype PitchRatio = PitchRatio (Ratio Int)
  deriving (Eq, Ord, Num, Show)

newtype Octave = Octave Int deriving (Eq, Ord, Show)

data Pitch = Pitch {
  ratio      :: PitchRatio,
  octave     :: Octave
  } deriving (Eq, Ord, Show)

newtype Velocity = Velocity Int deriving (Eq, Ord, Show)

newtype Duration = Duration (Ratio Int) deriving (Num, Eq, Ord, Show)

newtype TimePoint = TimePoint (Ratio Int) deriving (Num, Eq, Ord, Show)

newtype DissonanceScore = DissonanceScore (Ratio Int) deriving (Num, Eq, Ord, Show)

data Envelope = Impulse | Sustained deriving (Ord, Eq, Show)

data Instrument = Instrument {
  range :: (Int, Int)
  } deriving (Ord, Eq, Show)

data Sound = Sound {
  pitch       :: Pitch,
  velocity    :: Velocity,
  start       :: TimePoint,
  stop        :: TimePoint,
  minDuration :: Duration,
  maxDuration :: Duration,
  envelope    :: Envelope,
  instrument  :: Instrument
  } deriving (Ord, Eq, Show)

newtype Instruments = Instruments (Map Text Instrument) deriving Show

data SoundErrors =
  PitchRatioInvalid PitchRatio
  | VelocityRangeError Velocity
  | OctaveRangeError Octave
  | DurationRangeError Duration
  | NoInstrumentError Text
  deriving (Ord, Eq, Show)

data Chord = Chord {
  deltaDuration :: Duration,
  notes         :: [Sound]
  } deriving Show

instance A.FromJSON Instrument where
   parseJSON (A.Object v) =
     Instrument <$>
     v A..: "range"
   parseJSON _ = mzero

instance A.FromJSON Instruments where
    parseJSON val = Instruments <$> A.parseJSON val

mkPitchRatio :: Ratio Int -> V.Validation [SoundErrors] PitchRatio
mkPitchRatio ratio = bool
  (V.Failure [PitchRatioInvalid ratio'])
  (V.Success ratio')
  (ratio < 2)
  where ratio' = PitchRatio ratio

mkOctave :: Int -> V.Validation [SoundErrors] Octave
mkOctave octave =
  bool
  (V.Failure [OctaveRangeError octave'])
  (V.Success octave')
  (octave >= 0 && octave <= 10)
  where octave' = Octave octave

mkPitch :: Ratio Int -> Int -> V.Validation [SoundErrors] Pitch
mkPitch ratio octave =
  Pitch <$>
  mkPitchRatio ratio <*>
  mkOctave octave

mkVelocity :: Int -> V.Validation [SoundErrors] Velocity
mkVelocity velocity =
  bool
  (V.Failure [VelocityRangeError velocity'])
  (V.Success velocity')
  (velocity >= 0 && velocity <= 127)
  where velocity' = (Velocity velocity)

mkDuration :: Ratio Int -> V.Validation [SoundErrors] Duration
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
  -> Ratio Int
  -> Int
  -> Int
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
    pure Sustained <*>
    instrument'

soundDefault = Sound {
  pitch       = Pitch (PitchRatio (1 % 1)) (Octave 0),
  velocity    = Velocity 127,
  start       = TimePoint 0,
  stop        = TimePoint 1,
  minDuration = Duration 1,
  maxDuration = Duration 2,
  envelope    = Sustained,
  instrument  = Instrument { range = (0, 127) }
  }

melos :: Int -> Instruments -> V.Validation [SoundErrors] [Sound]
melos n instrumentMap =
  let pitchRatios = [1 % 1, 5 % 4, 3 % 2, 7 % 4]
      octaves = [1, 1, 2, 8, 1]
      velocities = [0, 1, 2, 8, 1]
      instruments = ["instrument_1"]
  in
    sequenceA $
    take n $
    getZipList $
    (mkSound instrumentMap) <$>
    ZipList (cycle pitchRatios) <*>
    ZipList (cycle octaves) <*>
    ZipList (cycle velocities) <*>
    ZipList (cycle instruments)

instrumentMapTest :: Instruments
instrumentMapTest =
  Instruments (Map.fromList [("instrument_1", Instrument { range = (0, 60) })])

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

getPitchRatios :: [Sound] -> [PitchRatio]
getPitchRatios = (distinct . map (ratio . pitch))

getDissonanceScore :: [PitchRatio] -> DissonanceScore
getDissonanceScore pitchRatios =
  case (count == 0) of
    True -> DissonanceScore (0 % 1)
    False ->
      DissonanceScore $
      (\x -> x % count) $
      sum $
      map (complexity . getInterval) $
      intervals
  where
    count = length intervals
    complexity ratio = numerator ratio + denominator ratio
    getInterval (PitchRatio x, PitchRatio y) = max x y / min x y
    intervals = distinct (pairs (distinct pitchRatios))

pairs :: (Foldable t1, Ord t) => t1 t -> [(t, t)]
pairs set = [(x,y) | let list = toList set, x <- list, y <- list, x < y]

data ResolutionErrors a =
  DissonanceResolutionError a
  | CountReductionError a
  | SoundMergeError a
  deriving (Eq, Show)

eitherResolve isValid process error xs =
  case (isValid xs) of
    True  -> Right xs
    False ->
      case (process xs) of
        Left result  -> Left (error result)
        Right result -> eitherResolve isValid process error result

eitherRemoveOne ::
  (t -> Bool) -> ([t] -> [t]) -> [t] -> Either [t] [t]
eitherRemoveOne partitionBy sortBy_ [] = Right []
eitherRemoveOne partitionBy sortBy_ xs =
  case (List.partition partitionBy xs) of
    ([], b) -> Left b
    (a, b)  -> Right (a' ++ b)
      where a' = (drop 1 . sortBy_) a

reduceDissonance ::
  TimePoint
  -> [PitchRatio]
  -> [Sound]
  -> Either (ResolutionErrors [Sound]) [Sound]
reduceDissonance now limit xs =
  eitherResolve
  ((<= limit') . dissonanceScore)
  (eitherRemoveOne (minDurationFilled now) (reverse . (sortBy (comparing start))))
  DissonanceResolutionError
  xs
  where limit'          = getDissonanceScore limit
        dissonanceScore = getDissonanceScore . map (ratio . pitch)

reduceCount ::
  TimePoint
  -> Int
  -> [Sound]
  -> Either (ResolutionErrors [Sound]) [Sound]
reduceCount now limit xs =
  eitherResolve
  ((<= limit) . length)
  (eitherRemoveOne (minDurationFilled now) (reverse . (sortBy (comparing start))))
  CountReductionError
  xs

addSound ::
  Sound
  -> [Sound]
  -> Either (ResolutionErrors [Sound]) [Sound]
addSound newSound xs =
  bool (Left (SoundMergeError merged)) (Right merged) canAdd
  where merged = xs ++ [newSound]
        params = (start Arrow.&&& pitch Arrow.&&& instrument)
        canAdd = (length merged) == (Set.size . Set.fromList . (map params)) merged

forwardTime now sounds = undefined

constraintsTest =
  (reduceDissonance (TimePoint 0) (Protolude.map PitchRatio [1 % 1, 3 % 2]))
  <=< (reduceCount (TimePoint 10) 5 $)
  <=< (addSound soundDefault)

pitches = [
  soundDefault {
      pitch = (Pitch (PitchRatio (1%1)) (Octave 4)),
      start = TimePoint 1,
      minDuration = Duration 1
      },
  soundDefault {
      pitch = (Pitch (PitchRatio (5%4)) (Octave 4)),
      start = TimePoint 1,
      minDuration = Duration 1
      },
  soundDefault {
      pitch = (Pitch (PitchRatio (3%2)) (Octave 4)),
      start = TimePoint 1,
      minDuration = Duration 1
      },
  soundDefault {
      pitch = (Pitch (PitchRatio (7%4)) (Octave 4)),
      start = TimePoint 1,
      minDuration = Duration 1
      }
  ]

main :: IO ()
main = do
  instrumentData <- (
    A.eitherDecode <$>
    B.readFile "resources/instruments.json"
    ) :: IO (Either [Char] Instruments)

  (count:_) <- getArgs

  case (melos <$>
        (readEither count :: Either [Char] Int) <*>
        instrumentData) of
    Left error -> print error
    Right (V.Failure errors) -> print (Set.fromList errors)
    Right (V.Success sounds) -> mapM_ print sounds
