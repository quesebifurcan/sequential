{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sequential where

import Protolude
import GHC.Show

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.DList as DL
import Control.Monad.State

newtype PitchRatio = PitchRatio (Ratio Integer)
  deriving (Enum, Eq, Ord, Num, Show)

newtype Octave = Octave Integer
  deriving (Eq, Ord, Show)

data Pitch = Pitch {
  ratio      :: PitchRatio,
  octave     :: Octave
  } deriving (Eq, Ord, Show)

newtype Velocity = Velocity (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

newtype Duration = Duration (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

newtype TimePoint = TimePoint {
  timePoint :: Ratio Integer
  } deriving (Num, Eq, Ord, Show)

newtype DissonanceScore = DissonanceScore (Ratio Integer)
  deriving (Num, Eq, Ord, Show)

data Envelope = Impulse | Sustained
  deriving (Ord, Eq, Show)

-- data Instrument = Instrument {
--   range :: (Int, Int)
--   } deriving (Ord, Eq, Show)

newtype Instrument = Instrument Text
  deriving (Eq, Ord, Show)

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

instance Aeson.FromJSON Instrument where
   parseJSON (Aeson.Object v) =
     Instrument <$>
     v Aeson..: "range"
   parseJSON invalid = AesonTypes.typeMismatch "Instrument" invalid

instance Aeson.FromJSON Instruments where
    parseJSON val = Instruments <$> Aeson.parseJSON val

data Moment = Moment {
  _now :: TimePoint
  , _active :: Set Sound
  , _result :: DL.DList Sound
  } deriving (Ord, Eq, Show)

momentDefault :: Moment
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
  [(x,y) | let xs = toList set, x <- xs, y <- xs, x < y]

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
applyDecay' moment@(Moment now active _) =
  case (eitherRemoveSound' moment) of
    Left _  -> applyDecay' (moment { _now = nextTimePoint })
    Right x -> x
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
reduceCount' limit moment@(Moment _ active _) =
  bool (reduceCount' limit (applyDecay' moment)) moment test
  where test = (Set.size active <= limit)

reduceDissonance_ :: Set PitchRatio -> Moment -> Moment
reduceDissonance_ limit moment@(Moment _ active _) =
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
  -- TODO: prior to `addSound'`, remove sounds,
  -- the maxDuration of which would be exceeded by the delta
  -- of the new sound.
  where dissonanceLimit' = (dissonanceLimit . constraint) sound
        maxCount'        = (maxCount . constraint) sound

resolveMoment :: Moment -> Sound -> Moment
resolveMoment moment sound = resolveConstraint moment
  where resolveConstraint = buildConstraint sound

fadeOut :: Moment -> Moment
fadeOut moment@(Moment _ active _) =
  bool (fadeOut $ applyDecay' moment) moment (active == Set.empty)

-- TODO: rename
getTimePoint :: TimePoint -> Duration -> TimePoint
getTimePoint now duration =
  let (TimePoint now')     = now
      (Duration duration') = duration
  in TimePoint (now' + duration')

-- TODO: rename
getNextSilence' :: Sound -> TimePoint
getNextSilence' sound =
  let (TimePoint start')      = start sound
      (Duration minDuration') = minDuration sound
  in TimePoint (start' + minDuration')

getNextSilence = head . sort . Set.toList . (Set.map getNextSilence')

-- TODO: rename
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

-- Flexible ordering + backtracking part of the solution?

data Parts = A | B | C | D deriving (Ord, Enum, Eq, Show)

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs

-- desiredOrder :: [Parts]
-- desiredOrder = [A, B, C]

data Sound' = Sound' { part :: Text, p :: Int, end :: Bool } deriving (Ord, Eq, Show)

-- sequences = Map.fromList [
--   (A, [
--       Sound' 1 False
--       , Sound' 2 False
--       , Sound' 3 True
--       ]),
--   (B, [
--       Sound' 1 False
--       , Sound' 2 True
--       ]),
--   (C, [
--       Sound' 1 True
--       , Sound' 1 False
--       , Sound' 2 False
--       , Sound' 3 True
--       ])
--   ]

-- getAndRotate k m = do
--   currSeq <- Map.lookup k m
--   currVal <- head currSeq
--   newMap  <- return $
--     Map.insert k (rotate' 1 currSeq) m
--   return (currVal, newMap)

isValid xs = (sum $ fmap p xs) <= 3

-- sequences = [
--   Sound' "a" 1 False
--   , Sound' "a" 2 False
--   , Sound' "a" 3 True
--   , Sound' "b" 1 False
--   , Sound' "b" 2 True
--   , Sound' "c" 1 True
--   , Sound' "c" 1 False
--   , Sound' "c" 2 False
--   , Sound' "c" 3 True
--   ]

-- When reaching an invalid state:
--   1. Remove most recently added event and try to add all optional events
--   a) If one of the optional events succeeds, accept that as a valid result
--   b) If no optional event succeeds, return Failure

-- head $ dropWhile (> 0) $ fmap (\x -> x + 1) [3,4,2,3,1,2,3]

data Sequences = Sequences {
  a :: [Sound']
  , b :: [Sound']
  , c :: [Sound']
  } deriving (Eq, Show)

initState = Sequences {
  a = [
      Sound' "a" 1 False
      , Sound' "a" 2 False
      , Sound' "a" 3 True
      ],
  b = [
      Sound' "b" 1 False
      , Sound' "b" 2 True
      ],
  c = [
      Sound' "c" 1 True
      , Sound' "d" 1 False
      , Sound' "e" 2 False
      , Sound' "f" 3 True
      ]
  }


data ResolutionStatus a = Resolved a | Pending a | Unresolvable a
  deriving (Eq, Show)

-- data InitialState a b = InitialState {
--   seqs       :: [a]
--   , coll       :: [b]
--   , lastSuccess :: Maybe (InitialState a b)
--   -- TODO: carry around the state of last resolved state
--   } deriving (Eq, Show)

-- append' s@(InitialState curr' _ _ _ _) x = s { curr = curr' ++ [x] }

-- TODO: simplified version with strings -- how simulate a state which *might*
-- get invalid at a later point in time?
-- Check 1: assert that text is in set
-- Check 2: assert that texts exactly match e.g. "cba"
asdf = [
  ["a", "b", "c"]
  , ["b", "c", "a"]
  , ["c", "a", "b"]
  , ["a", "b", "c"]
  ]

result' = ["c", "a", "a"]
isResolved x = x == (take (length x) result')
isValidEndResult x = x == result'

-- values = [
--   [Resolved, Pending, Unresolvable]
--   , [Pending, Unresolvable, Resolved]
--   , [Unresolvable, Resolved, Pending]
--   , [Resolved, Pending, Unresolvable]
--   ]

values = (take 5 (cycle [
  [Unresolvable 1, Unresolvable 2, Pending 3]
  , [Pending 4, Pending 5]
  , [Unresolvable 6, Unresolvable 7, Resolved 8]
  , [Unresolvable 18, Resolved 10, Pending 11]
  , [Resolved 12, Pending 13]
  ]))

-- -- Drawback: not all events might appear
-- run'' [] (Right coll) = Right coll
-- run'' (x:xs) (Right coll) =
--   let options = fmap (inner run'') x
--       inner f x =
--         case x of
--           Resolved     result -> f xs (Right (coll ++ [result]))
--           Pending      result -> f xs (Right (coll ++ [result]))
--           Unresolvable result -> Left coll
--             -- case result > 14 of
--             --   True -> f xs (run'' [[Unresolvable (result - 1), Resolved (result - 1)]] (Right (coll ++ [result])))
--             --   False -> Left coll
--   in
--     case (head $ dropWhile isLeft options) of
--       Just x -> x
--       Nothing -> Left []

-- No value is invalid in itself
keys' = [
  [1, 2, 3]
  , [1, 2, 3]
  -- , [1, 2, 3]
  -- , [1, 2, 3]
  -- , [2, 3, 1]
  -- , [3, 1, 2]
  -- , [1, 2, 3]
  -- , [2, 3, 1]
  -- , [3, 1, 2]
  -- , [1, 2, 3]
  -- , [2, 3, 1]
  -- , [3, 1, 2]
  ]

values' = Map.fromList ([
  (1, [6, 2, 2])
  , (2, [2, 3, 4])
  , (3, [0])
  -- , (3, [10, 10, 10])
  ])

getAndRotate k m = do
  currSeq <- Map.lookup k m
  currVal <- head currSeq
  newMap  <- return $
    Map.insert k (rotate' 1 currSeq) m
  return (currVal, newMap)

-- Notation of "pending" events? Use map with id as key -- replace
-- Insert single-segment sound:

-- Move existing sounds in group to coll and replace group with new
insertInGroup = undefined

-- run2 keys' values' (Right (0, [])) (keys', values', 0, [])
run2 [] events (Right (curr, coll)) (oldSeqs, oldEvents, oldCurr, oldColl) = Right (curr, coll)
run2 asdf@(x:xs) events (Right (curr, coll)) (oldSeqs, oldEvents, oldCurr, oldColl) =
  let options = zipWith (\a b -> (inner events a b)) x [0..]
      inner events x' index =
        let maybeNew = (getAndRotate x' events)
        in
          case maybeNew of
            Just (currVal, newMap) ->
                case currVal == 0 of
                  True ->
                    let newXs = if index == 0
                          then xs
                          else (x:xs)
                        newMap' = if index == 0
                          then newMap
                          else events
                    in
                      run2
                        newXs
                        newMap'
                        (Right (0, coll ++ [currVal]))
                        (newXs, newMap', 0, coll ++ [currVal])
                  False ->
                      -- Do not move on to next step in desired-order list
                      -- if the option we are dealing with is not the "desired" one
                        let newXs = if index == 0
                              then xs
                              else (x:xs)
                            newMap' = if index == 0
                              then newMap
                              else events
                        in
                          -- Important: no value should be invalid in itself
                          case (curr + currVal) <= 6 of
                            True -> run2
                                    newXs
                                    newMap'
                                    (Right (curr + currVal, coll ++ [currVal]))
                                    (oldSeqs, oldEvents, oldCurr, oldColl)
                            False -> Left (curr, coll)
            Nothing -> panic "asdf"
  in
    case (head $ dropWhile isLeft options) of
      Just x -> x
      Nothing ->
        let (b, c) = case (head oldSeqs, drop 1 oldSeqs) of
              (Nothing,  _) -> panic (Protolude.show oldSeqs)
              (Just [], c') -> panic "asdflkj"
              (Just b', c') -> (drop 1 b', c')
        in
          run2 (b:c) oldEvents (Right (oldCurr, oldColl)) ((b:c), oldEvents, oldCurr, oldColl)
                   -- run2 (b:c) oldEvents (Right (oldCurr, oldColl)) ((b:c), oldEvents, oldCurr, oldColl)

data TestState = TestState {
  seqs :: [Int]
  , coll :: [Int]
  } deriving (Eq, Show)

-- runState (mapM_ run3 [1,2,3]) (TestState [] [])

getStatus x = head x

-- TODO: separate state for `lastSuccess`
run4 :: State TestState ()
run4 = do
  initial@(TestState a b) <- get
  case a of
    [] -> app 9877777777777
    (x:xs) -> do
      modify (\y -> y { coll = (coll y) ++ [x] })
      app' a
      modify (\y -> y { seqs = xs })
      oijoij x
      oijoij x
      oijoij x
      oijoij x
      oijoij x
      run4
  where
    oijoij x =
      case (asdf' x) of
        Just (Right result) -> app (818181818 + result)
        Nothing -> return ()
    app :: Int -> State TestState ()
    app x = do
      modify (\y -> y { coll = (coll y) ++ [x] })
    app' :: [Int] -> State TestState ()
    app' x = do
      modify (\y -> y { coll = (coll y) ++ x ++ x ++ x })
    -- asdf :: [Int] -> State TestState ()
    asdf' num =
        head
        $ dropWhile isLeft
        $ fmap (\x -> if num == x then Right num else Left num) [7,6,5,4,3]
    asdf'' :: Maybe (Either b b) -> State TestState ()
    asdf'' x = do
      modify (\y -> y { coll = (coll y) ++ [321] })

data SegmentationFn = Open | Extend | Close deriving (Ord, Eq, Show)

data Event = Event { pitch' :: Int, nextFn :: Maybe SegmentationFn }
  deriving (Ord, Eq, Show)

-- Sound takes its "real" duration from its container; its values never change once initialized
-- Q: How record duration, start- and end-points of individual group events? Update values
-- when moving from `group__active` to `group__result`?

newtype Sound2 = Sound2 Integer deriving (Ord, Eq, Show)
data Chord = Chord {
  sounds :: (Set Sound2)
  , chord__start :: Maybe Int
  , chord__stop :: Maybe Int
  } deriving (Ord, Eq, Show)

data Group = Group {
  group__id :: Integer
  , group__seq :: [Chord]
  , group__active :: Maybe Chord -- only one event active at any time
  , group__result :: [Chord]
  } deriving (Eq, Show)

data Moment2 = Moment2 {
  moment__now :: Int
  , moment__seq :: [Group]
  , moment__active :: Set Group
  , moment__result :: [Group]
  } deriving (Eq, Show)

-- Only one group with the same id can be active at any time.
-- instance Ord Group where
--   compare = (comparing group__id)

------------------------------------
-- Overlapping Pitches
data Sound3 = Sound3 {
  sound3__pitch :: Int
  , sound3__delta :: Int
  , crescType :: Int
  } deriving (Ord, Eq, Show)

setDeltas delta [] coll = coll
setDeltas delta (x:[]) coll = setDeltas delta [] (coll ++ [x { sound3__delta = delta }])
setDeltas delta (x:xs) coll = setDeltas delta xs (coll ++ [x { sound3__delta = 0 }])

qwerqwer =
  let input = snd $ execState (testSequence (Sound3 1 0 100) (Sound3 2 0 200) (Sound3 3 0 300)) (Set.empty, [])
  in
    fmap (\x -> setDeltas 4 (Set.toList x) []) input

testSequence2 = [[0], [0, 2], [0, 2, 4], [2, 4], [4]]

testSequence :: Sound3 -> Sound3 -> Sound3 -> State (Set Sound3, [Set Sound3]) ()
testSequence a b c = do
  add a
  tick
  add b
  tick
  remove a
  add c
  tick
  remove b
  tick
  return ()

  where
    add :: Sound3 -> State (Set Sound3, [Set Sound3]) ()
    add x = modify $ \(curr, coll) -> (Set.insert x curr, coll)
    tick :: State (Set Sound3, [Set Sound3]) ()
    tick = modify $ \(curr, coll) -> (curr, coll ++ [curr])
    remove :: Sound3 -> State (Set Sound3, [Set Sound3]) ()
    remove x = modify $ \(curr, coll) -> (Set.delete x curr, coll)

-- End Overlapping Pitches
------------------------------------

------------------------------------
-- Multi-segment Sounds

-- delta durations are coupled to the smallest type in each group
-- dissonance contribution: check state of `group4__active`
-- each group has an insert function: e.g. one or more segements can be moved from seq to active in each phase
-- relative insert times can be used in group: first elt is 0, its precise
-- onset depending on the Group's onset in the containing Moment.

-- data Chord4 = Chord4 {
--   chord4__sounds :: Set Sound4
--   , chord4__start :: Int
--   , chord4__stop :: Int
--   } deriving (Eq, Show)

data Group4 = Group4 {
  group4__now      :: Int
  , group4__seq      :: [Sound4]
  , group4__active :: Map Int Sound4
  , group4__result :: [Sound4]
  } deriving (Ord, Eq, Show)

data Sound4SegmentType = Open4 | Extend4 | Close4 deriving (Ord, Eq, Show)

data Sound4 = Sound4 {
  sound4__pitch   :: Int
  , sound4__id    :: Int
  , sound4__start :: Int
  , sound4__stop  :: Int
  , sound4__delta :: Int
  , sound4__type  :: Sound4SegmentType
  } deriving (Ord, Eq, Show)

-- data Sound4Segments = Sound4Segment {
--   sound4Segment__seq      :: [Sound4]
--   , sound4Segment__active :: Set Sound4
--   , sound4Segment__result :: [Set Sound4]
--   } deriving (Eq, Show)

data Moment4 = Moment4 {
  moment4__now      :: Int
  , moment4__groups :: Map Int [Group4]

  , moment4__seq    :: [Int]
  , moment4__active :: Map Int Group4
  , moment4__result :: [Group4]
  } deriving (Eq, Show)

-- instance Ord Sound4 where
--   compare = comparing sound4__id

phrase1 = [
  Sound4 0 123 0 0 1 Open4
  , Sound4 0 123 0 0 1 Extend4
  , Sound4 0 123 0 0 1 Close4
  ]

phrase2 = [
  Sound4 1 234 0 0 2 Open4
  , Sound4 1 234 0 0 2 Extend4
  , Sound4 1 234 0 0 2 Close4
  ]

phrase3 = [
  Sound4 2 345 0 0 2 Open4
  , Sound4 2 345 0 0 2 Extend4
  , Sound4 2 345 0 0 2 Close4
  ]

--TODO: function for converting list of groups to sequence of groups

group1_4 = Group4 0 (phrase1 ++ phrase2) Map.empty []
group2_4 = Group4 0 phrase3 Map.empty []
allGroups = Map.fromList [
  (0, [group1_4])
  , (1, [group2_4])
  ]
moment1_4 = Moment4 0 allGroups [0,1,1,1,1,0,0,0,0,0,0,0,0,0] Map.empty []

-- TODO: Sound is seq of sound segments

-- how get the next group in sequence
-- 1. Add new group
-- 2. Modify existing group

-- Should sequence of groups be [Group] instead of [GroupID]?

-- maybeInsert k id' elem active
--       (x:xs) = seq
--       (removed, newActive) = Map.insertLookupWithKey
--           (\_ newValue _ -> newValue)
--           (sound4__id x)
--           x
--           active

-- NOTE: since "multiple inserts via single index" can now be controlled
-- from the inside of a Group, perhaps `moment4__seq` can be a simple list?

-- data Groups4 a = Groups4 {
--   groups4__keys :: [Int],
--   groups4__groups :: Map Int [a]
--   } deriving (Eq, Show)

-- moment4__getNextGroup m@(Moment4 now groups (x:xs) active result) = do

-- try taking the first, if success, then rotate. if failure, then
-- take second etc.

runMoment4__getGroups m@(Moment4 now groups (x:xs) active result) =
  case Map.lookup x active of
    Just nextGroup -> (nextGroup, groups)
    Nothing        ->
      case getAndRotate' x groups of
        Just (nextGroup, nextGroups) -> (nextGroup, nextGroups)
        Nothing                      -> panic "lookup failure"

runMoment4 m@(Moment4 now groups [] active result) =
  m { moment4__result = result ++ (Map.elems active) }
runMoment4 m@(Moment4 now groups (x:xs) active result) =
  let (nextGroup, nextGroups) = runMoment4__getGroups m
      nextGroup' = runGroup4 (nextGroup { group4__now = now })
      (activeGroups, resolvedGroups) = Map.partition
        (\x -> group4__active x == Map.empty &&
               group4__seq x == [])
        active
  in
    Moment4
    (group4__now nextGroup')
    nextGroups
    xs
    (Map.insert x nextGroup' activeGroups)
    (result ++ (Map.elems resolvedGroups))

printGroup4 =
  (map (\x -> (sound4__pitch x, sound4__id x, sound4__type x, sound4__stop x))) . group4__result

resultTest n =
  case head $ drop n $ take (n+1) (iterate runMoment4 moment1_4) of
    Just result -> mapM_ (print . printGroup4) $ (moment4__active result)
    Nothing -> panic "oijsdf"
-- Insert group

--   let (a, b) = (Map.lookup x active, getAndRotate' x groups)
--       (nextGroup, nextGroups) =
--         case (a, b) of
--           (Just a', _) -> (
--             runGroup4 (a' { group4__now = now })
--             , groups
--             )
--           (_, Just (b', b'')) -> (
--             runGroup4 (b' { group4__now = now })
--             , b''
--             )
--       (removed, newActive) = Map.insertLookupWithKey
--           (\_ newValue _ -> newValue)
--           x
--           nextGroup
--           active
--   in
--     case removed of
--       Just removed' -> Moment4
--                        (group4__now nextGroup)
--                        nextGroups
--                        xs
--                        newActive


        -- fmap (\x -> runGroup4 x { group4__now = now }) (Map.lookup

  -- | otherwise =
  --   case getAndRotate' x groups of
  --         Just (a, b) -> (runGroup4 a { group4__now = now }, b)
  --         Nothing     -> panic "oij"
  --   in
  --     Moment4
  --       (group4__now nextGroup)
  --       nextGroups
  --       xs
  --       (Map.insert x nextGroup active)
  --       result

  --   where (x:xs) = seq

getAndRotate' k m = do
  currSeq <- Map.lookup k m
  currVal <- head currSeq
  newMap  <- return $
    Map.insert k (rotate' 1 currSeq) m
  return (currVal, newMap)

-- λ> mapM_ (print . fmap sound4__pitch . Set.toList) $ group4__result $ runGroup4 group1_4
-- []
-- [0]
-- [0,1]
-- [0,0,1]
-- [0,0,1,1]
-- [0,0,0,1,1]
-- [0,0,0,1,1,1]

-- Split by: groupBy ((/=) `on` sound4__id)
-- unique by: mapM_ print $ nubBy ((==) `on` sound4__id) [ Sound4 4 234 0 0 1 Open4, Sound4 4 234 1 0 1 Open4 ]

-- NB: not all runGroup functions should be recursive (recursion means that
-- all sounds are inserted right away). Most should just forward the group
-- one tick and return it.

-- Q: use map?
-- It makes sense for the sounds to have unique ids -- set operations also make
-- a lot of sense. But is there a way to avoid having to invent names for all
-- materials/phrases/groups? At the moment, the main sequence is a list of
-- indices; is there a better way?
-- One possible solution would be to use a list of partially evaluated
-- "get-next-state" functions which are applied to a Group at a later
-- point in time.
-- Or, just pass in the group itself.
-- A: Interestingly, a Map seems suitable for Group as well as Sound -- and
-- it would be nice to use the same data structure in both places.
--
-- Place logic for backtracking *outside* of runX functions

data ResolutionStatus4 a = Pending4 a | Resolved4 a
  deriving (Eq, Show)

runGroup4 g@(Group4 now seq active result)
  | seq == [] && (active == Map.empty) = g
  | seq == [] = Group4 now [] Map.empty (result ++ (fmap (\x -> x { sound4__stop = now } ) (Map.elems active)))
  | otherwise =
    case removed of
      Just removed' -> Group4 now' xs newActive (result ++ [removed' { sound4__stop = now }])
      Nothing       -> Group4 now' xs newActive result
    where
      (x:xs) = seq
      now' = now + sound4__delta x
      (removed, newActive) = Map.insertLookupWithKey
          (\_ newValue _ -> newValue)
          (sound4__id x)
          x
          active

               -- (sound4__id x) x active) (result ++ removed)
    -- where
    --   (x:xs) = seq
    --   removed = case Set.lookupIndex x active of
    --     Just index -> [Set.elemAt index active]
    --     Nothing    -> []

-- nextActiveGroup sound sounds = undefined

-- phrase = [
--   Set.fromList [
--       Sound4 0 1 0 0
--       ]
--   , Set.fromList [
--       Sound4 0 1 0 0
--       , Sound4 2 1 0 0
--       ]
--   , Set.fromList [
--       Sound4 2 1 0 0
--       ]
--   , Set.fromList [
--       Sound4 2 1 0 0
--       , Sound4 4 1 0 0
--       ]
--   , Set.fromList [
--       Sound4 4 1 0 0
--       ]
--   ]

-- End Multi-segment Sounds
------------------------------------

------------------------------------
-- Separate insertion and unfolding?
-- In other words -- how make a regular pulse be equivalent to a sustained sound?
-- But -- is it really necessary? It adds additional complexity -- perhaps there
-- is a way to encode this behavior into the sounds themselves?
-- It would definitely be more elegant to have "polyrhythms" arise naturally out of the interaction
-- between sounds.
-- IFF that is possible that all, the next question would be: what is the most minimal
-- gestural language which is still sufficiently expressive?

data Sound5 = Sound5 { sound5__pitch :: Int } deriving (Eq, Show)

-- pitch:  [0, 2, 0, 2]
-- cresc:  [startCresc, startCresc, stopCresc, stopCresc]
-- delta:  [1 0 1 x]
-- insert: [add add

------------------
-- Flat structure (single set with isolated sounds):
-- + simpler structure, less types to juggle
-- + no concept of a tie, just sounds with a start and end
-- + more extensible
-- - more complicated rules
-- - less explicit

-- Is group in group set:
-- head $ Set.filter ((== 3) . group__id)  (Set.fromList [Group 3 [] Nothing [] ])

-- test: multi-segment chord phrase with synced crescendo
-- (pitch, delta, crescStep)
-- [(c, 0, startCresc), (d, 1, startCresc), (c, 0, startDim), (d, 1, startDim)]
-- Multi-segment sound: when inserting a sound with id x in set, check if id x is already present.
-- If so, then update sound properties with new event (e.g. "start cresc",  etc.)

testSyncChordCresc :: [Int] -> [Int] -> [Char] -> [(Int, Int, Char)]
testSyncChordCresc pitches deltas crescStates =
  let pitchCount = length pitches
      count      = pitchCount * length crescStates
      pitches'   = take count (cycle pitches)
      deltaHead  = take (count - 1) (repeat 0)
      deltas'    = concat (map (\x -> concat [deltaHead, [x]]) (cycle deltas))
      cresc      = concat (map (\x -> (take pitchCount $ repeat x)) (cycle crescStates))
  in
    take count $
    getZipList $
    ((,,) <$> ZipList pitches' <*> ZipList deltas' <*> ZipList cresc)

  -- where
  --   app x = modify (\s -> s ++ x)

newtype Active = Active (Set Group) deriving (Eq, Show)

momentTest =
  Moment2 {
    moment__now = 0
    , moment__seq = [
        Group {
            group__id = 1
            , group__seq = [
                Chord (Set.fromList([Sound2 0])) Nothing Nothing
                , Chord (Set.fromList([Sound2 0, Sound2 2])) Nothing Nothing
                , Chord (Set.fromList([Sound2 0, Sound2 2, Sound2 4])) Nothing Nothing
                ]
            , group__active = Nothing
            , group__result = []
            }
        , Group {
            group__id = 2
            , group__seq = [
                Chord (Set.fromList([Sound2 5 , Sound2 6])) Nothing Nothing
                , Chord (Set.fromList([Sound2 7 , Sound2 8])) Nothing Nothing
                ]
            , group__active = Nothing
            , group__result = []
            }
        ]
    , moment__active = Set.empty
    , moment__result = []
    }

-- runMoment m@(Moment2 _ []     _ _) = m
-- runMoment m@(Moment2 now (x:xs) active result) =
--   runMoment
--   $ Moment2 (now + 1) xs (Set.insert x active) (result ++ Set.toList active)

interleave = concat . transpose
combos [] = [[]]
combos ([]:ls) = combos ls
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)

makeChord defaultDelta delta [] coll = coll
makeChord defaultDelta delta xs coll =
  f xs []
  where
    f []     coll = coll
    f (x:[]) coll = f [] (coll ++ [(x, delta)])
    f (x:xs) coll = f xs (coll ++ [(x, defaultDelta)])

-- makeChordPhrase delta [] coll = coll
-- makeChordPhrase delta xs coll =
--   f xs []
--   where
--     f []     coll = coll
--     f (x:xs) coll = f xs (coll ++ (makeChord delta x []))

addEvent x xs = Set.insert x xs
removeEvent x xs = Set.delete x xs

makePhrase' t a b c (x:xs) =
  f xs [t x a]
  where
    f [] coll = coll
    f (x:[]) coll = f [] (coll ++ [t x c])
    f (x:xs) coll = f xs (coll ++ [t x b])

makePhrase xs =
  makePhrase' Event (Just Open) (Just Extend) (Just Close) xs
