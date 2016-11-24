{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Data.Either.Validation as V
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

newtype Pitch = Pitch (Ratio Int) deriving (Eq, Ord, Show)
newtype Duration = Duration (Ratio Int) deriving (Eq, Ord, Show)

data Envelope =
  Impulse
  | Sustained
  deriving Show

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
  pitch      :: Pitch,
  duration   :: Duration,
  envelope   :: Envelope,
  instrument :: Text
  } deriving Show

data SoundErrors =
  PitchOutOfRange
  | DurationOutOfRange
  | NoInstrumentError Text
  deriving Show

data Chord = Chord {
  deltaDuration :: Duration,
  notes         :: [Sound]
  } deriving Show

mkSound ::
  Pitch
  -> Duration
  -> Text
  -> Instruments
  -> V.Validation [SoundErrors] Sound
mkSound pitch duration instrument instrumentMap =
  let pitch'    = bool (V.Failure [PitchOutOfRange]) (V.Success pitch) (pitch < (Pitch 2))
      duration' = bool (V.Failure [DurationOutOfRange]) (V.Success duration) (duration > (Duration 0))
      instrument' = case (instrumentExists instrument instrumentMap) of
        Just _ -> V.Success instrument
        Nothing -> V.Failure [NoInstrumentError instrument]
  in Sound <$>
     pitch' <*>
     duration' <*>
     pure Impulse <*>
     instrument'

instrumentExists k (Instruments m) = Map.lookup k m

main :: IO ()
main = do
  d <- (A.eitherDecode <$> B.readFile "resources/instruments.json") :: IO (Either [Char] Instruments)
  case d of
    Left err -> print err
    Right instrumentMap -> print $ mkSound (Pitch (11 % 8)) (Duration (1 % 4)) "instrument_1" instrumentMap

