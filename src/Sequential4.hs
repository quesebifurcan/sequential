{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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



data Moment2 = Moment2 {
  now      :: Int
  , active :: Set (Int, Int)
  , result :: [(Int, Int)]
  } deriving (Ord, Eq, Show)

insert x moment@(Moment2 now active _) =
  moment { active = (Set.insert new active) }
  where
    (a, b) = x
    new    = (now, b)

tick moment@(Moment2 now _ _) = moment { now = (now + 1) }

collect moment@(Moment2 now active result) =
  Moment2 now Set.empty (result ++ coll)
  where
    coll = fmap (\(start, stop) -> (start, now)) $
           Set.toList active

run :: State Moment2 ()
run = do
  modify (insert (0, 0))
  modify tick
  modify (insert (0, 0))
  modify tick
  modify collect
  modify (insert (0, 0))
  modify tick
  modify tick
  modify tick
  modify tick
  modify collect
  return ()

type Moment2__T e m a = StateT Moment2 (ExceptT e m) a

runMoment2__T :: (Monad m) => Moment2__T e m a -> Moment2 -> m (Either e a)
runMoment2__T m = runExceptT . evalStateT m

type Moment2__ e a = Moment2__T e Identity a
runMoment2__ m = runIdentity . runMoment2__T m

run2 :: Moment2__ Moment2 Moment2
run2 = do
  modify (insert (0, 0))
  modify tick
  modify tick
  modify tick
  unless (4 > 3) (get >>= Error.throwError)
  modify collect
  modify tick
  modify (insert (0, 0))
  get
