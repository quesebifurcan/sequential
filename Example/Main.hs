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

toFloat x = fromRational x :: Float

printQuoted = PP.enclose PP.squote PP.squote . PP.string

printSound :: Int -> Sound -> PP.Doc
printSound nodeId sound =
  PP.list [PP.float start', PP.list expr]
  where start'                                     = fromRational $ ((timePoint . start) sound)
        stop'                                      = fromRational $ ((timePoint . stop) sound)
        duration                                   = stop' - start'
        baseFreq                                   = 55 % 1
        (Pitch (PitchRatio ratio) (Octave octave)) = pitch sound
        frequency                                  = toFloat $ baseFreq * (octave % 1) * ratio
        (Velocity velocity')                       = (velocity sound)
        expr = [
          printQuoted "s_new"
          , printQuoted "sine"
          , PP.int nodeId
          , PP.int 0
          , PP.int 0
          , printQuoted "frequency"
          , PP.float frequency
          , printQuoted "duration"
          , PP.float duration
          ]

printSounds :: [Sound] -> PP.SimpleDoc
printSounds sounds = PP.renderOneLine . PP.list $ zipWith printSound [1000..] sounds

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
  , instrument    = Instrument "sine"
  }

main :: IO ()
main = do
  instrumentData <- (
    A.eitherDecode <$>
    B.readFile "resources/instruments.json"
    ) :: IO (Either [Char] Instruments)

  -- (count:_) <- getArgs

  -- case (melos <$>
  --       (readEither count :: Either [Char] Int) <*>
  --       instrumentData) of
  --   Left error -> print error
  --   Right (V.Failure errors) -> print (Set.fromList errors)
  --   Right (V.Success sounds) -> mapM_ print sounds

  -- withFile "test.txt" WriteMode $ (\h -> PP.hPutDoc h (printSounds [soundDefault, soundDefault]))

  -- print $ map _timePoint $ setDeltaDurations $ soundsToMidiEvents $ _result $ run' (take 400 (cycle pitches))

  -- print instrumentData

  withFile "testing-example.txt" WriteMode $ (\h -> PP.displayIO h (printSounds (DL.toList $ _result $ run' pitches)))
