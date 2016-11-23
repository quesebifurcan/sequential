{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude

type Pitch = Ratio Int
type Duration = Ratio Int

data Note = Note {
  pitch      :: Pitch,
  duration   :: Duration,
  instrument :: Text
  } deriving Show

data Chord = Chord {
  deltaDuration :: Duration,
  notes         :: [Note]
  } deriving Show

makeChord :: Pitch -> Duration -> Duration -> Text -> Chord
makeChord pitch duration deltaDuration instrument = Chord {
  deltaDuration = deltaDuration,
  notes = [
      Note {
          pitch = pitch,
          duration = duration,
          instrument = instrument
          }
      ]
  }

main :: IO ()
main = do
  print $ makeChord (7 % 4) 1 2 "Clarinet"
