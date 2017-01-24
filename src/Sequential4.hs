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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.DList as DL
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Error.Class as Error
import Control.Monad.Trans.Either
import Control.Monad.Loops (whileM_)

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
  , sound__label           :: Text
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
  , sound__label = "A"
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

data Events a b = Events {
  events__curr :: b
  , events__keys :: [a]
  , events__map :: Map a [b]
  } deriving (Eq, Show)

data Moment = Moment {
  moment__now      :: TimePoint
  , moment__events :: Events Text Sound
  , moment__active :: Set Sound
  , moment__result :: [Sound]
  }
  deriving Show

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

-- Use ranges for the different params (Pitch etc.)
-- When exceeding a range, switch to next group (or do something else)

type VerticalId = Int
type HorizontalId = Int
type IDTag = (Text, HorizontalId, VerticalId)

gestureC :: State (Int, [Int], IDTag) ()
gestureC = do
  nextId
  upA 3
  upA 1
  (_, result, _) <- get
  if length result >= 13
     then return ()
     else gestureC
  where
    up :: (Int, Int) -> Int -> (State (Int, [Int], IDTag)) ()
    up (lo, hi) n = do
      replicateM_ n (modify f)
      modify (\(curr, result, count) -> (curr, result ++ [curr], count))
      where
        f (curr, result, count) =
          case (curr < lo, curr > hi) of
            (True, _) -> (hi, result, count)
            (_, True) -> (lo, result, count)
            _         -> (curr + 1, result, count)
    -- TODO: splice in ranges
    upA = up (20, 30)

    nextId = modify f
      where f (curr, result, (text, hId, vId)) = (curr, result, (text, hId + 1, vId))

runGestureC :: Int -> Text -> (Int, [Int], IDTag)
runGestureC start label = snd $ runState gestureC (start, [], (label, 0, 0))

-- zipLists -> Map ->
-- Data.List.zipWith runGestureC [20, 25] ["oij", "asdf"]

buildMap = Map.fromList . zipWith (,) [0..]

data IndexedScale = IndexedScale {
  scale_k :: Integer
  , scale_range :: (Integer, Integer)
  , scale_m :: Map Integer Integer
  } deriving Show

transposeScale f (IndexedScale k range@(lo, hi) m) =
  case (k' < lo, k' > hi) of
    (True, _) -> Left $ IndexedScale hi range m
    (_, True) -> Left $ IndexedScale lo range m
    _         -> Right $ IndexedScale k' range m
  where k' = f k

-- up = transposeScale (+1)
-- down = transposeScale (subtract 1)

scaleTest = IndexedScale 0 (0, 5) (buildMap [43, 56, 53, 23, 64, 45])

data ScaleTest = ScaleTest {
  __scale :: IndexedScale
  , __coll :: [Integer]
  } deriving Show

up :: Int -> State ScaleTest ()
up n = replicateM_ n (modify f)
  where f      s@(ScaleTest scale coll) =
          either (leftA s) (rightA s) (transposeScale (+1) scale)
        leftA  s@(ScaleTest _ coll) x   = s { __scale = x, __coll = coll ++ [100] }
        rightA s@(ScaleTest _ coll) x   = s { __scale = x }

coll :: State ScaleTest ()
coll = modify f
  where f s@(ScaleTest (IndexedScale k _ m) coll) =
          case Map.lookup k m of
            Nothing     -> panic "..."
            Just result -> s { __coll = coll ++ [result] }

gestureD :: State ScaleTest ()
gestureD = do
  coll
  up 4
  coll
  up 4
  coll
  -- coll
  -- up 4
  -- coll

    -- Just x  -> (x, IndexedScale k' range m)
    -- Nothing -> up 0 (IndexedScale lo range m)

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

resolvePending = undefined
canResolve = undefined
-- canResolve: all pending events can be resolved. If false, trigger
-- forceResolve.

-- addSound sound m@(Moment now active result) =
--   case canResolve merged of
--     Right result -> result
--     Left _       -> addSound sound (forceResolve m)
--   where merged = undefined
--         forceResolve = undefined

-- Moment date past present future

getPitchRatios = Set.map (pitch__ratio . sound__pitch)

isConsonant limit (Moment _ _ active _) =
  dissonanceScore (getPitchRatios active) <= limit

-- Use when e.g. applyDecay always works (no pending sounds)
tryResolve1 ::
  (Moment -> Maybe Moment)
  -> (Moment -> Moment)
  -> Moment
  -> Moment
tryResolve1 f g m =
  case f m of
    Just result -> result
    Nothing     -> tryResolve1 f g (g m)

-- next element vs. next member element
-- Use when short-circuiting (use Either instead?)
tryResolve2 :: (Moment -> Maybe Moment) -> Moment -> Maybe Moment
tryResolve2 f m = f m

toDuration (TimePoint x) = Duration x
toTimePoint (Duration x) = TimePoint x

maxDurationExceeded :: TimePoint -> Sound -> Bool
maxDurationExceeded now sound =
  now - start >= maxDuration
  where
    getMaxDuration       = toTimePoint . sound__maxDuration
    (start, maxDuration) = (sound__start sound, getMaxDuration sound)

minDurationFilled :: TimePoint -> Sound -> Bool
minDurationFilled now sound =
  (start + minDuration) - now <= 0
  where
    getMinDuration       = toTimePoint . sound__minDuration
    (start, minDuration) = (sound__start sound, getMinDuration sound)

mustRemove now sound = undefined -- maxDurationExceeded now sound
canRemove now sound = undefined -- minDuration filled, group resolved
nextToRemove moment = undefined -- oldest sound for which `canRemove` is True

rotate' n xs = zipWith const (drop n (cycle xs)) xs

getAndRotate k m = do
  currSeq <- Map.lookup k m
  currVal <- head currSeq
  newMap  <- return $
    Map.insert k (rotate' 1 currSeq) m
  return (currVal, newMap)

data NextEventStatus a b = Success a | Failure b | Done a
  deriving (Eq, Show)

-- Use "curr event" as field on Moment?
getNextEvent e@(Events _ [] _) = Done e
getNextEvent (Events curr (k:ks) m) =
  case getAndRotate k m of
    Just (currVal, newMap) -> Success $ Events currVal ks newMap
    Nothing                -> Failure "lookup error"

mkEvents (k:ks) m = do
  (curr, m') <- getAndRotate k m
  return $ Events curr ks m'

-- asdf =
--   let events = mkEvents ["a", "b"] (Map.fromList [("a", [1,2,3]), ("b", [4,5,6])])
--   in
--     case events of
--       Just result -> Just $ (runEitherT . runState) $ run5 result
--       Nothing -> Nothing
events = mkEvents ["a", "b", "a"] (Map.fromList [("a", [1,2,3]), ("b", [4,5,6])])

data Run5State = Run5State {
  events5 :: Events Text Int
  , coll5 :: [Int]
  } deriving Show

run7 :: EitherT Text (State Run5State) ()
run7 = do
  (Run5State a b) <- get
  put (Run5State a [8,8,8,8,8,8,8,8])
  return ()

run5 :: EitherT Text (State Run5State) ()
run5 = do
  a <- get'''
  coll
  whileM_ ((\x -> length x > 0) <$> (gets (events__keys . events5))) $ do
    nextEvent
    a <- get'''
    coll
  (Run5State a b) <- get

  -- Inner function using part of state
  eights <- return $ (execState . runEitherT) run7 (Run5State a [])
  modify (\(Run5State a b) -> Run5State a (b ++ (coll5 eights)))

  return ()

  where
    get''' = gets (events__curr . events5)
    -- nextEvent :: EitherT Text (State ((Events Int), [Int])) ()
    nextEvent = do
      a <- gets events5
      case getNextEvent a of
        Success nxt -> modify (\s -> s { events5 = nxt })
        Failure err -> left err
        Done _ -> left "done"

    coll :: EitherT Text (State Run5State) ()
    coll = modify f
      where f s@(Run5State a b) = s { coll5 = b ++ [events__curr a] }

asdf =
  case events of
    Just result -> (runState . runEitherT) run5 (Run5State result [])
    Nothing -> panic "no data"

-- Using whileM_ to improve control flow
run6 :: State (Int, [Int]) ()
run6 = do
  replicateM_ 6 (modify (\(a, b) -> (a + 1, b)))
  whileM_ ((\x -> x < 10) <$> gets fst) $ do
    modify (\(a, b) -> (a + 1, b ++ [123]))
  return ()

data Counter = Counter {
  counter__soundId :: Int
  , counter__groupId :: Int
  } deriving Show

counterDefault = Counter 0 0

runGesture :: State (Counter, Int, [(Int, Int)]) ()
runGesture = do
  modify incr
  modify coll
  modify coll
  modify incr
  modify coll
  where
    incr (a, b, c) = (a, b + 1, c)
    coll ((Counter sId gId), b, c) =
      ((Counter (sId + 1) gId), b, c ++ [(sId, b)])

-- TODO: collect results as map?
runGestures :: State (Counter, [(Text, [(Int, Int)])]) ()
runGestures = do
  runSub "a" runGesture
  runSub "b" runGesture
  runSub "c" runGesture
  return ()
  where
    runSub ::
     Text
     -> State (Counter, Int, [(Int, Int)]) ()
     -> State (Counter, [(Text, [(Int, Int)])]) ()
    runSub label f = do
      (counter, coll) <- get
      let (l, (a, b, c)) = (label, snd $ runState f (counter, 0, []))
      modify (\(counter, coll) -> (a, coll ++ [(l, c)]))
      return ()

isDissonant = undefined

isRemovable = undefined

insertSound = undefined

anyRemovable = undefined

anyResolved = undefined

allResolved = undefined

isPending = undefined

applyDecay = undefined

forwardTime = undefined

reduceDissonance = undefined

reduceCount = undefined

getNextMinDuration = undefined

isMember = undefined

getGroups = undefined
