{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Data.Either.Validation as V
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

newtype PitchRatio = PitchRatio (Ratio Int) deriving (Eq, Ord, Show)
newtype Octave = Octave Int deriving (Eq, Ord, Show)
newtype Duration = Duration (Ratio Int) deriving (Eq, Ord, Show)
newtype Onset = Onset (Ratio Int) deriving (Eq, Ord, Show)
newtype TimePoint = TimePoint (Ratio Int) deriving (Eq, Ord, Show)

data Pitch = Pitch {
  ratio      :: PitchRatio,
  octave     :: Octave
  } deriving (Eq, Ord, Show)

data Envelope =
  Impulse
  | Sustained
  deriving (Eq, Show)

data Instrument = Instrument {
  range :: (Int, Int)
  } deriving Show

instance A.FromJSON Instrument where
   parseJSON (A.Object v) =
     Instrument <$>
     v A..: "range"
   parseJSON _ = mzero

newtype Instruments =
  Instruments (Map Text Instrument)
  deriving Show

instance A.FromJSON Instruments where
    parseJSON val = Instruments <$> A.parseJSON val

data Sound = Sound {
  pitch       :: Pitch,
  start       :: TimePoint,
  stop        :: TimePoint,
  minDuration :: Duration,
  maxDuration :: Duration,
  envelope    :: Envelope,
  instrument  :: Text
  } deriving (Eq, Show)

soundDefault = Sound {
  pitch       = Pitch (PitchRatio 1) (Octave 0),
  start       = TimePoint 0,
  stop        = TimePoint 1,
  minDuration = Duration 1,
  maxDuration = Duration 1,
  envelope    = Sustained,
  instrument  = "default"
  }

data SoundErrors =
  PitchOutOfRangeError
  | DurationOutOfRangeError
  | NoInstrumentError Text
  deriving Show

data Chord = Chord {
  deltaDuration :: Duration,
  notes         :: [Sound]
  } deriving Show

isValidPitch (Pitch (PitchRatio r) (Octave o)) =
  let ratioValidation = r >= 1 && r < 2
      octaveValidation = o >= 0 && o < 10
  in ratioValidation && octaveValidation

mkSound pitch duration instrument instrumentMap =
  let pitch'    = bool (V.Failure [PitchOutOfRangeError]) (V.Success pitch) (isValidPitch pitch)
      duration' = bool (V.Failure [DurationOutOfRangeError]) (V.Success duration) (duration > (Duration 0))
      instrument' = case (instrumentExists instrument instrumentMap) of
        Just _ -> V.Success instrument
        Nothing -> V.Failure [NoInstrumentError instrument]
  in Sound <$>
    pitch' <*>
    pure (TimePoint 1) <*>
    pure (TimePoint 1) <*>
    pure (Duration 1) <*>
    pure (Duration 2) <*>
    pure Impulse <*>
    instrument'

instrumentExists k (Instruments m) = Map.lookup k m

instrumentMapTest =
  Instruments (Map.fromList [("instrument_1", Instrument { range = (0, 60) })])

melody =
  let pitchRatios = (cycle [1 % 1, 5 % 4, 3 % 2, 7 % 4])
      durations = (cycle [1 % 4, 1 % 8, 1 % 16])
      f ::
        Ratio Int
        -> Ratio Int
        -> V.Validation [SoundErrors] Sound
      f p d = mkSound
        (Pitch (PitchRatio p) (Octave 0))
        (Duration d)
        "instrument_1"
        instrumentMapTest
  in
    f <$>
      ZipList pitchRatios <*>
      ZipList durations


main :: IO ()
main = do
  instrumentData <- (A.eitherDecode <$> B.readFile "resources/instruments.json") :: IO (Either [Char] Instruments)
  case instrumentData of
    Left err -> print err
    Right instrumentMap ->
      print $
      mkSound
      (Pitch (PitchRatio (11 % 8)) (Octave 0))
      (Duration (1 % 4))
      "instrument_1"
      instrumentMap
