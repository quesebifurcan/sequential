{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Protolude
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

import Sequential

--
-- Smart Constructors
--
octaveEquivalentRatio r
  | r < 1     = octaveEquivalentRatio (r * 2)
  | r >= 2    = octaveEquivalentRatio (r / 2)
  | otherwise = r

isValidPitchRatio (PitchRatio ratio) =
  numerator ratio >= 0 && denominator ratio >= 1

isValidPitch (Pitch ratio (Octave octave)) =
  isValidPitchRatio ratio

data SoundErrors =
  PitchRatioInvalid PitchRatio
  | VelocityRangeError Velocity
  | OctaveRangeError Octave
  | DurationRangeError Duration
  | NoInstrumentError Text
  deriving (Ord, Eq, Show)

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

--
-- Materials
--

slope :: (Ord t, Floating t, Enum t) => t -> t -> [t]
slope exponent count =
  let range' = map (\x -> x ** exponent) [0..count]
      max' = maximum range'
      range'' = map (/ max') range'
  in (List.init . drop 1) range''

-- a = Protolude.map (\x -> (x, 'c')) $ slope 1.9 5
-- b = Protolude.map (\x -> (x, 'd')) $ slope 1.7 8
-- c = Protolude.map (\x -> (x, 'e')) $ slope 1.5 13
-- d = Protolude.map (\x -> (x, '.')) $ slope 1.3 87
-- e = map snd $ sort $ a ++ b ++ c ++ d

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

soundDefault = Sound {
  pitch           = Pitch (PitchRatio (1 % 1)) (Octave 0)
  , velocity      = Velocity 127
  , start         = TimePoint 0
  , stop          = TimePoint 1
  , minDuration   = Duration 1
  , maxDuration   = Duration 2
  , deltaDuration = Duration 1
  , constraint    = MomentConstraint { dissonanceLimit = Set.empty
                                     , maxCount = 4 }
  , envelope      = Sustained
  , instrument    = Instrument "Testing"
  }

pitches = [
  soundDefault {
      pitch = (Pitch (PitchRatio (1%1)) (Octave 4)),
      start = TimePoint 0,
      minDuration = Duration 1,
      deltaDuration = Duration 1
      },
  soundDefault {
      pitch = (Pitch (PitchRatio (5%4)) (Octave 4)),
      start = TimePoint 0,
      minDuration = Duration 1,
      deltaDuration = Duration (2)
      },
  soundDefault {
      pitch = (Pitch (PitchRatio (3%2)) (Octave 4)),
      start = TimePoint 2,
      minDuration = Duration 1,
      deltaDuration = Duration (3)
      },
  soundDefault {
      pitch = (Pitch (PitchRatio (7%4)) (Octave 4)),
      start = TimePoint 0,
      minDuration = Duration 1,
      deltaDuration = Duration (4)
      }
  ]

main :: IO ()
main = do
  instrumentData <- (
    A.eitherDecode <$>
    B.readFile "resources/instruments.json"
    ) :: IO (Either [Char] Instruments)
  print soundDefault

-- Test

-- data ResolutionState = Pending | Resolved deriving (Show)

-- data Sound' = Sound' {
--   pitch' :: Int
--   , minDuration' :: Int
--   , next :: Maybe Sound'
--   } deriving (Ord, Eq, Show)

-- data Moment' = Moment' {
--   coll :: [Sound]
--   } deriving (Ord, Eq, Show)

-- a x = Sound' {
--   asdf = 1 + x
--   , count = 0
--   , next = Just Sound' {
--       asdf = 2 + x
--       , count = 0
--       , next = Just Sound' {
--           asdf = 3 + x
--           , count = 0
--           , next = Nothing
--           }
--       }
--   }

-- isResolved moment =
--   all (fmap isResolved' moment)

-- isResolved' sound =
--   case sound of
--     Sound' _ _ (Just _) -> False
--     Sound' _ _ Nothing  -> True

-- sounds' = [a 0, a 1, a 2, a 3]

-- asdfSum xs = sum $ map asdf xs

-- applyDecay'' xs = fmap (\x -> let c = count x
--                         in x { count = c + 1 })
--                  xs

-- -- move (coll, curr) =
-- --   let candidates = reverse (sortBy (comparing count)
-- --                             (filter (\x -> count x > 3)
-- --                              (filter isResolved' curr)))
-- --   in
-- --     case candidates of
-- --       [] -> (coll, curr)
-- --       (x:_) -> (coll ++ [x]

-- qwer (coll, curr) x =
--   let result = curr ++ [x]
--   in (coll ++ result, curr)

-- runSounds xs =
--   foldl' qwer
--   ([], [])
--   xs
