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
          PP.string "'s_new'"
          , PP.string "'sine'"
          , PP.int nodeId
          , PP.int 0
          , PP.int 0
          , PP.string "'frequency'"
          , PP.float frequency
          , PP.string "'duration'"
          , PP.float duration
          ]

printSounds :: [Sound] -> PP.SimpleDoc
printSounds sounds = PP.renderOneLine . PP.list $ zipWith printSound [1000..] sounds

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
