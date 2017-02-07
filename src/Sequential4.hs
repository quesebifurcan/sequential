{-# LANGUAGE FlexibleContexts #-}
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
import qualified Control.Arrow as Arrow
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Error.Class as Error
import Control.Monad.Trans.Either
import Control.Monad.Loops (orM, whileM_, whileM, untilM_)
import Control.Monad.Logic

import Data.Validation as Validation

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
  events__curr   :: b
  , events__keys :: [a]
  , events__map  :: Map a [b]
  } deriving (Eq, Show)

data Group a b = Group {
  group__id        :: a
  , group__pending :: [b]
  , group__active  :: Set b
  , group__result  :: [b]
  , group__resolutionPriority :: Int
  } deriving (Ord, Eq, Show)

isPendingGroup = not . null . group__pending

nextGroupState now g@(Group _ pending active _ _) =
  case head pending of
    Just result -> Right $
      g {
      group__pending = drop 1 pending
      , group__active = Set.insert (result { sound__start = now }) active
      }
    Nothing     -> Left g

-- insertGroup group m@(Moment now c

firstStart xs =
  head . sort . fmap sound__start . Set.toList $ xs

lowestBy f = sortBy (comparing f)

hasActive (Group _ _ active _ _) = not $ null active

getLeastRecentlyUpdated groups
  | null active = Nothing
  | otherwise   = min' active
  where
    min'     = head . sortBy (comparing getStart)
    getStart = fmap sound__start . Set.toList . group__active
    active   = filter hasActive groups

nextGroup m@(Moment now [] active result) = m
nextGroup m@(Moment now (g:gs) active result) = m

data Moment = Moment {
  moment__now      :: TimePoint
  , moment__events :: [Group Int Sound]
  , moment__active :: Set (Group Int Sound)
  , moment__result :: [Sound]
  } deriving Show

getAndRotate k m = do
  currSeq <- Map.lookup k m
  currVal <- head currSeq
  newMap  <- return $
    Map.insert k (rotate' 1 currSeq) m
  return (currVal, newMap)

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

getPitchRatios = Set.map (pitch__ratio . sound__pitch)

-- applyDecay :: Moment -> Moment
-- applyDecay m@(Moment now _ active _) =
--   m { moment__now = nextTimePoint }
--   where nextTimePoint =
--           case (getNextSilence active) of
--             Nothing -> now
--             Just x -> x

isSoundResolved sound active =
  (not (Set.member sound active)) ||
  -- not $ Set.member sound active ||
  any (Set.member sound) resolvedGroups
  -- any (Set.member sound) [active]
  where
    resolvedGroups = filter isGroupResolved (getGroups active)

canRemove now x xs =
  minDurationFilled now x && isSoundResolved x xs

filterCanRemove now xs =
  Set.filter (\x -> canRemove now x xs) xs

groupId x = (sound__verticalGroup x, sound__horizontalGroup x)

getByGroupId sound active =
  -- Set.member (groupId sound) (Set.map groupId active)
  head $ Set.filter (\x -> groupId sound == groupId x) active

noActive m@(Moment _ _ active _) = active == Set.empty

-- -- TODO: insert n sounds into active (no checks applied?)
-- -- Use "bypassing" functions in Test.hs
-- forceResolve :: State Moment ()
-- forceResolve = do
--   modify removeAllRemovable
--   a <- get
--   bool
--     (modify applyDecay)
--     (gets moment__events >>= return . getNext >>= modify)
--     (allPending . moment__active $ a)
--   curr <- get
--   ks <- gets (events__keys . moment__events)
--   -- if (noActive curr || ks == [])
--   if noActive curr
--      then return ()
--      else forceResolve
--   where
--     getNext x = (\m ->
--                 let active = moment__active m
--                 in
--                   m { moment__events = getNextEvent' active x }
--                   )

    -- getNextEvent'' m@(Moment _ events active _) =
    --   case getNextEvent' active events of
    --     Just x -> m { moment__events = x }
    --     Nothing -> m

-- Resolution of Groups independent of seq?

-- removeAllRemovable m@(Moment now events active result) =
--   removeSounds (filterCanRemove now active) m

-- removeSounds toRemove m@(Moment now events active result) =
--   m {
--   moment__active = newActive
--   , moment__result = result ++ (Set.toList removed)
--   }
--   where newActive = Set.difference active toRemove
--         setStop x = x { sound__stop = now }
--         removed   = Set.map setStop toRemove

-- insert sound: check that removed sounds satisfy:
--   MinimumDurationConstraint -> applyDecay
-- and, afterwards, that new state satisfies:
--   DissonantConstraint -> removeUntil
--   MaximumCountConstraint -> removeUntil
--   IsResolvableConstraint -> graduallyResolveUntil


-- resolvePending = undefined
-- canResolve = undefined

isConsonant limit active =
  dissonanceScore (getPitchRatios active) <=
  dissonanceScore limit

isGroupResolved :: Foldable t => t Sound -> Bool
isGroupResolved = any ((== Resolved) . sound__status)

allPending :: Foldable t => t Sound -> Bool
allPending = all ((== Pending) . sound__status)

getResolvedGroups :: [Set Sound] -> [Set Sound]
getResolvedGroups = filter isGroupResolved

allResolved active = all isGroupResolved

-- filterMaxDurationExceeded = undefined
-- canMerge = undefined
-- tryResolve = undefined
-- mergeNext = undefined

-- run :: State Moment ()
-- run = do
--   filterMaxDurationExceeded
--   untilM_ canMerge tryResolve
--   mergeNext

getGroups :: Set Sound -> [Set Sound]
getGroups =
  fmap Set.fromList . f . Set.toList
  where
    id         = sound__horizontalGroup
    group'     = (==) `on` id
    sort'      = sortBy (comparing id)
    f          = List.groupBy group' . sort'

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

-- isDissonant = undefined

-- isRemovable = undefined

-- insertSound = undefined

-- anyRemovable = undefined

-- anyResolved = undefined

-- isPending = undefined

-- forwardTime = undefined

-- reduceDissonance = undefined

-- reduceCount = undefined

-- getNextMinDuration = undefined

-- isMember = undefined

rotate' n xs = zipWith const (drop n (cycle xs)) xs

-- data NextEventStatus a b = Success a | Failure b | Done a
  -- deriving (Eq, Show)

-- getNextEvent' m@(Moment _ (Events _ [] _) _ _) = Done m
-- getNextEvent' m@(Moment _ (Events curr (k:ks) eventsMap) active _) =
--   case getAndRotate k eventsMap of
--     Just (currVal, newMap) ->
--       Success $ m { moment__events = Events currVal ks newMap }
--     Nothing                -> Failure "lookup error"

-- TODO: use Maybe for this?
-- If not, all `nextEvent` functions can be simplified
-- and whileM can easily be used

-- getNextEvent e@(Events _ [] _) = Nothing
-- getNextEvent (Events curr (k:ks) m) =
--   case getAndRotate k m of
--     Just (currVal, newMap) -> Just $ Events currVal ks newMap
--     Nothing                -> panic "lookup error"

-- getNextEvent' active events =
--   case getNextEvent events of
--     Just nextState ->
--       if isInActiveGroup (events__curr nextState) active
--          then Just nextState
--          else getNextEvent' active nextState
--     Nothing -> Nothing

-- getNextEvent e@(Events _ [] _) = Nothing
-- getNextEvent (Events curr (k:ks) m) =
--   case getAndRotate k m of
--     Just (currVal, newMap) -> Just $ Events currVal ks newMap
--     Nothing                -> panic "lookup error"

getCurr e@(Events _ [] _) = Nothing
getCurr e@(Events curr _ _) = Just curr

getNextEvent e@(Events _ [] _) = e
getNextEvent (Events curr (k:ks) m) =
  case getAndRotate k m of
    Just (currVal, newMap) -> Events currVal ks newMap
    Nothing                -> panic "lookup error"

-- getNextEvent' active events =
--   if isInActiveGroup (events__curr nextState) active
--     then nextState
--     else getNextEvent' active nextState
--   where nextState = getNextEvent events

getNextEvent' active e@(Events _ [] _) = e
getNextEvent' active e@(Events curr (k:ks) currMap)
  | null active || null ks = e
  | otherwise =
    case getAndRotate k currMap of
      Just (currVal, newMap) ->
        if isInActiveGroup currVal active
          then Events currVal ks newMap
          else getNextEvent' active (Events curr ks currMap)
      Nothing                -> panic "lookup error"

isInActiveGroup :: Sound -> Set Sound -> Bool
isInActiveGroup sound active =
  Set.member a s
  where
    a = sound__horizontalGroup sound
    s = Set.map sound__horizontalGroup active

mkEvents (k:ks) m = do
  (curr, m') <- getAndRotate k m
  return $ Events curr ks m'

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

-- run5 :: EitherT Text (State Run5State) ()
-- run5 = do
--   a <- get'''
--   coll
--   whileM_ ((\x -> length x > 0) <$> (gets (events__keys . events5))) $ do
--     nextEvent
--     a <- get'''
--     coll
--   (Run5State a b) <- get

--   -- Inner function using part of state
--   eights <- return $ (execState . runEitherT) run7 (Run5State a [])
--   modify (\(Run5State a b) -> Run5State a (b ++ (coll5 eights)))

--   return ()

--   where
--     get''' = gets (events__curr . events5)
--     -- nextEvent :: EitherT Text (State ((Events Int), [Int])) ()
--     nextEvent = do
--       a <- gets events5
--       case getNextEvent a of
--         Success nxt -> modify (\s -> s { events5 = nxt })
--         Failure err -> left err
--         Done _ -> left "done"

--     coll :: EitherT Text (State Run5State) ()
--     coll = modify f
--       where f s@(Run5State a b) = s { coll5 = b ++ [events__curr a] }

-- asdf =
--   case events of
--     Just result -> (runState . runEitherT) run5 (Run5State result [])
--     Nothing -> panic "no data"

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
-- run: runState runGestures (counterDefault, DL.empty)
-- (length . snd .snd) $ runState runGestures (counterDefault, DL.empty)
runGestures :: State (Counter, DL.DList (Text, [(Int, Int)]) ) ()
runGestures = do
  replicateM_ 10000 $ do
    runSub "a" runGesture
    runSub "b" runGesture
    runSub "c" runGesture
    runSub "c" runGesture
    runSub "c" runGesture
    runSub "c" runGesture
  return ()
  where
    runSub ::
     Text
     -> State (Counter, Int, [(Int, Int)]) ()
     -> State (Counter, DL.DList (Text, [(Int, Int)])) ()
    runSub label f = do
      (counter, coll) <- get
      let (l, (a, b, c)) = (label, snd $ runState f (counter, 0, []))
      modify (\(counter, coll) -> (a, DL.snoc coll (l, c)))
      return ()

------------------------------------------------------------
-- 1. Use Silence
-- 2. Gestures as do blocks. Actions: up/down, new phrase when out of range,
-- nextDuration, sample etc.
-- spanning several phrases/groups, loop over finite sets of elements

-- data GestureState = GestureState {
--   -- __scale :: [Int]
--   -- , durs :: [Duration]
--   vels :: [Velocity]
--   , groupId :: Int
--   } deriving (Show)

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

--------------------------------------------------------------

data GroupAppend = GroupLegato | GroupSustain
  deriving (Ord, Eq, Show)

data GroupRemove = GroupRemoveAll | GroupRemoveOne
  deriving (Ord, Eq, Show)

data Moment' a b = Moment' {
  moment'__now       :: a
  , moment'__pending :: [b]
  , moment'__active  :: Set b
  , moment'__result  :: [b]
  } deriving (Eq, Show)

data Group' a b = Group' {
  group'__id                   :: a
  , group'__pending            :: [b]
  , group'__active             :: Set b
  , group'__result             :: [b]
  , group'__resolutionPriority :: Int
  , group'__onAppend           :: GroupAppend
  , group'__onRemove           :: GroupRemove
  } deriving (Eq, Show)

instance Ord GroupAlias
  where compare x y = compare (group'__id x) (group'__id y)

data Event' a b = Event' {
  event'__start               :: TimePoint
  , event'__stop              :: TimePoint
  , event'__minDuration       :: Duration
  , event'__maxDuration       :: Duration
  , event'__delta             :: Duration
  , event'__value             :: a
  , event'__transitionParams  :: b
  } deriving (Ord, Eq, Show)

data Event'Append = Event'Legato | Event'Sustain
  deriving (Ord, Eq, Show)

data Event'Remove = Event'RemoveAll | Event'RemoveOne
  deriving (Ord, Eq, Show)

data Event'Params = Event'Params {
  event'Params__maxCount :: Int
  , event'Params__maxSum :: Int
  , event'Params__onAppend :: Event'Append
  , event'Params__onRemove :: Event'Remove
  } deriving (Ord, Eq, Show)

nextGroupState' now g@(Group' _ [] active _ _ _ _) = Right g
nextGroupState' now g@(Group' _ (x:xs) active _ _ _ _) =
  bool (Left g) (Right nextState) (isGroupValid nextState)
  where
    nextState = g {
      group'__pending = xs
      , group'__active = Set.insert (x { event'__start = now }) active
      }

data A = A { a__maxCount :: Int, a__type :: Event'Append }
data B = B { b__maxCount :: Int, b__diss :: Int, b__type :: Event'Append }

class NextStateTest a where
  appendTo   :: a -> (Moment' b c -> Moment' b c)
  isValidMoment :: a -> Moment' b c -> Bool

instance NextStateTest A where
  appendTo (A x _) =
    (\y -> case length (moment'__pending y) > x of
        True -> let active = moment'__active y
                in
                  y { moment'__result = Set.toList active
                    , moment'__active = Set.empty }
        False -> y { moment'__pending = [] })
  isValidMoment (A x _) (Moment' _ _ active _) = Set.size active < x

-- TODO: how encode variations `removeOne` and `removeAll`?
instance NextStateTest B where
  appendTo (B x _ Event'Legato) =
    (\y -> case length (moment'__active y) > x of
        True -> y
        False -> y { moment'__pending = [] })
  appendTo (B x _ Event'Sustain) = identity
  isValidMoment (B x y _) (Moment' _ pending active _) =
    length pending > x && Set.size active < y

-- instance NextStateTest B where
--   appendTo (B x y z) = (\xx -> x + y + z + xx)
  -- removeFrom (B x y z) = (\xx -> xx - (x + y + z))

isGroupValid  g = True
isMomentValid m = True
resolveMoment m = undefined
resolveGroup g = undefined
-- getNextGroup m = undefined

type EventAlias = Event' Int Event'Params
type GroupAlias = Group' Int EventAlias
type MomentAlias = Moment' TimePoint GroupAlias

memberBy f x xs = Set.member (f x) (Set.map f xs)

-- runMoment' [] m = m

-- TODO: same resolution strateg(y/ies) for Group and Moment?
-- runMoment' :: EitherT MomentAlias (State MomentAlias) ()

-- nextState''' :: EitherT MomentAlias (State MomentAlias) ()
-- nextState''' = do
--   -- get next group
--   return ()

-- runMoment' m@(Moment' now [] active result)
--   | active == Set.empty = m
--   | otherwise = m {
--       -- TODO: this is the "removal" phase; it can be customized by each Event.
--       moment'__active = Set.empty
--       , moment'__result = result ++ Set.toList active
--       }

-- runMoment' m@(Moment' now groups@(g:gs) active result)
--   | memberBy group'__id g active = runMoment' $ nextState'''' m
--   | otherwise =
--     if isValidMoment merged
--         then merged { moment'__pending = gs }
--         else runMoment' $ nextState'''' m
--   where
--     isValidMoment m = True -- TODO: unfold and check all states
--     merged = m { moment'__active = Set.insert g active }

-- anyPending m@(Moment' now pending active result) = not (null pending)
-- anyActive m@(Moment' now pending active result) = not (null active)
-- nextPendingIsMember m@(Moment' now pending active result) =
--   memberBy <$> pure group'__id <*> (head pending) <*> pure active
-- canResolveMergedState m = True

-- nextState2 m =
--   case (moveNextPendingToActive, nextGroupState) of
--     (Just result, _) -> result
--     (Nothing, Just result) -> result
--     _ -> m
--   where
--     moveNextPendingToActive = undefined
--     nextGroupState = undefined

-- isValidMerge m@(Moment' now [] active result) =

-- moveNextPendingToActive m@(Moment' now [] active result) = Left m

-- data MomentErrors = NoPending | IsMember deriving (Eq, Show)

-- data MomentStatus a b =
--   MomentSuccess a
--   | MomentFailure b
--   deriving (Eq, Show)

addNextGroup :: MomentAlias -> Maybe MomentAlias
addNextGroup m@(Moment' now pending active result) = do
  group <- head pending
  guard $ not (memberBy group'__id group active)
  merged <- return $ merge group m
  guard $ canResolve merged
  return merged
  where
    merge group m@(Moment' _ pending active _) = m {
      moment'__pending = drop 1 pending
      , moment'__active = Set.insert group active
      }
    canResolve m = True

addNextSound = undefined
addNextSoundWithConstraints = undefined
-- removeSound = undefined
-- removeSoundWithConstraints = undefined
-- removeGroup = undefined

sortByResolutionPriority =
  sortBy (comparing group'__resolutionPriority) . Set.toList

split l = case head l of
  Just result -> Just (result, tailSafe l)
  Nothing -> Nothing

removeFromGroupAt now toRemove g@(Group' _ _ active result _ _ _) =
  g {
  group'__active   = Set.delete toRemove active
  , group'__result = result ++ [setStop now toRemove]
  }

setStart now event = event { event'__start = now }
setStop now event = event { event'__stop = now }

isEmptyGroup group =
  (null . group'__active $ group) && (null . group'__pending $ group)

removeEvent :: MomentAlias -> Maybe MomentAlias
removeEvent m@(Moment' now _ active _) = do
  (g, gs)   <- splitByNextGroup active
  (e, es)   <- splitByNextEvent g
  timePoint <- return $ bool now (getNextSilence' e) (minDurationFilled' now e)
  newGroup  <- return $ removeFromGroupAt timePoint e g
  -- newActive <- return $ Set.union (Set.singleton newGroup) (Set.fromList gs)
  newActive <- return $ Set.insert newGroup active
  return $ m { moment'__now = timePoint, moment'__active = newActive }
  where
    splitByNextGroup groups = split . sortByResolutionPriority $
      Set.filter (not . null . group'__active) groups
    splitByNextEvent group = split . Set.toList . group'__active $ group

maybeGet f = maybe mzero return f
setActive x m = m { moment'__active = x }
setResult x m = m { moment'__result = x }

-- runMoment m@(Moment' now pending active result) =
--   case (groupToAdd, eventToAdd, eventToRemove, groupToRemove) of
--     (Just group, _,                   _,                   _)          -> m
--     (Nothing,    Just (group, event), _,                   _)          -> m
--     (Nothing,    Nothing,             Just (group, event), _)          -> m
--     (Nothing,    Nothing,             Nothing,             Just group) -> m
--     (Nothing,    Nothing,             Nothing,             Nothing)    -> m
--   where
--     groupToAdd    = head $ filter (\x -> not . memberBy group'__id x $ active) pending
--     groups        = sortByResolutionPriority $ active
--     groupToRemove = head groups
--     eventToAdd    = eventToModify group'__pending
--     eventToRemove = eventToModify group'__active
--     eventToModify f = do
--       group <- head . filter (not . null. f) $ groups
--       event <- head . f $ group
--       return (group, event)

appendLegato g = undefined
appendSustain = undefined

handleNewGroup g m = m { moment'__active = Set.insert g (moment'__active m) }
handlePendingGroup g m = m { moment'__active = Set.insert g (moment'__active m) }
-- handlePendingGroup g m =
--   -- TODO: incremented `now`?
--   where
--     updateFn = case group'__onAppend of
--       GroupLegato -> appendLegato
--       GroupSustain -> appendSustain
handleActiveGroup g m = m { moment'__active = Set.insert g (moment'__active m) }
handleEmptyGroup g m = m { moment'__active = Set.insert g (moment'__active m) }
handleNoGroup m = m

runMomentOnce m@(Moment' _ pending active _) =
  f newGroups     handleNewGroup <|>
  f pendingGroups handlePendingGroup <|>
  f activeGroups  handleActiveGroup <|>
  f emptyGroups   handleEmptyGroup &
  defaultGroup
  where
    hasPending     = not . null . group'__pending
    hasActive      = not . null . group'__active
    isMember group = memberBy group'__id group $ active
    allGroups      = sortByResolutionPriority $ active
    newGroups      = filter (not . isMember) pending
    pendingGroups  = filter hasPending allGroups
    activeGroups   = filter hasActive allGroups
    emptyGroups    = filter (\x -> not $ hasActive x || hasPending x) allGroups
    f x g          = g <$> head x <*> pure m
    defaultGroup   = maybe m identity

-- groupNextPending ::
--   (MonadState (Group' a b) m, MonadPlus m, Ord b) =>
--   (b -> b) -> m (Group' a b)

nextPending :: MaybeT (State Int) ()
nextPending = do
  maybe mzero return (head [])
  put 123
  -- a@(Moment' now pending active result) <- get
  -- g <- gets (head . sortByResolutionPriority . moment'__active)
  -- group  <- maybe mzero return
  return ()
  -- event' <- return $ eventF event
  -- modify (setPending $ drop 1)
  -- modify (setActive $ Set.insert event')
  -- return group
  -- where
  --   setPending f group = group { group'__pending = f . group'__pending $ group }
  --   setActive f group  = group { group'__active = f . group'__active $ group }

runPending m@(Moment' _ pending active _) = do
  -- allGroups     <- return $ sortByResolutionPriority $ active

  group <- head . filter hasPending . sortByResolutionPriority $ active
  event <- head . group'__pending $ group

  return m
  where
    hasPending     = not . null . group'__pending
    hasActive      = not . null . group'__active

data GroupStatus a =
  NewGroup a
  | PendingGroup a
  | ActiveGroup a
  | EmptyGroup a
  | NoGroup
  deriving (Eq, Show)

-- getGroupFn status =
--   case status of
--     NewGroup a
--     PendingGroup a
--     ActiveGroup a
--     EmptyGroup a
--     NoGroup

-- getGroupFn status =
--   groupFunction'__appendLegato    :: Group a b -> Group a b
--   , groupFunction'__appendSustain :: Group a b -> Group a b
--   , groupFunction'__removeAll     :: Group a b -> Group a b
--   , groupFunction'__removeOne     :: Group a b -> Group a b

-- runMomentOnce :: MomentAlias -> MomentAlias
-- runMomentOnce m@(Moment' _ pending active result) =
--   case getMomentOperation m of
--     NewGroup group ->
--       m { moment'__active = Set.insert group active }
--     PendingGroup group ->
--       let next = (group'__onAppend group) $ group
--       in m
--         -- m {
--         -- moment'__active = Set.insert next active
--         -- }
--     ActiveGroup group -> m
--     EmptyGroup group -> m
--     NoGroup -> m

removeEvent2 :: MaybeT (State MomentAlias) MomentAlias
removeEvent2 = do
  m@(Moment' now _ active _) <- get
  g <- maybeGet $ nextGroup active
  e <- maybeGet $ nextEvent g
  whileM_ (do now <- gets moment'__now
              return $ not . minDurationFilled' now $ e) $ do
    modify applyDecay'
  m@(Moment' now _ active _) <- get
  g' <- return $ removeFromGroup now e g
  modify (setActive $ Set.insert g' active)
  get >>= return
  where
    applyDecay' m = m { moment'__now = (moment'__now m) + (TimePoint 1000) }
    nextGroup groups = head . sortByResolutionPriority $
      Set.filter (not . null . group'__active) groups
    nextEvent group = head . Set.toList . group'__active $ group
    removeFromGroup now toRemove g@(Group' _ _ active result _ _ _) =
      g {
      group'__active   = Set.delete toRemove active
      , group'__result = result ++ [setStop now toRemove]
      }

removeEventFromGroup2 :: MaybeT (State GroupAlias) ()
removeEventFromGroup2 = do
  event <- gets (head . Set.toList . group'__active) >>= maybeGet

  return ()

removeGroup :: MomentAlias -> Maybe MomentAlias
removeGroup m@(Moment' _ _ active result) = do
  (g, gs)   <- splitByNextGroup active
  newActive <- return $ Set.delete g (Set.fromList gs)
  return $ m { moment'__active = newActive
             , moment'__result = result ++ [g] }
  where
    splitByNextGroup groups =
      split . sortByResolutionPriority $ Set.filter isEmptyGroup groups

addGroup :: MomentAlias -> Maybe MomentAlias
addGroup m@(Moment' now pending active result) = do
  group <- head pending
  guard (not (memberBy group'__id group active))
  return $ m { moment'__active = Set.insert group active }

getNextSilence' :: EventAlias -> TimePoint
getNextSilence' event =
  let (TimePoint start')      = event'__start event
      (Duration minDuration') = event'__minDuration event
  in TimePoint (start' + minDuration')

-- use state in order to be able to modify e.g. `now` directly from inside group

getNextSilence = head . sort . Set.toList . (Set.map getNextSilence')

minDurationFilled' now event =
  (start + minDuration) - now <= 0
  where
    getMinDuration       = toTimePoint . event'__minDuration
    (start, minDuration) = (event'__start event, getMinDuration event)

-- data MomentStatus a =
--   MomentResolved a
--   | MomentPending a
--   deriving (Eq, Show)

oij2 :: EitherT Int (State (Int, Int)) ()
oij2 = do
  -- whileM_ ((< 10) <$> get) (modify (+1))
  -- whenM ((== 10) <$> get) (get >>= left)
  -- x <- safeGet (head . moment'__pending)
  modify (\(x, y) -> (x, y + 100))
  left 10
  (a, b) <- get
  return ()
  where
    safeGet f = gets f >>= maybe (get >>= left) return

oij3 :: MaybeT (State Int) Int
oij3 = do
  a <- get
  -- guard (a < 10)
  maybe mzero return (head [])
  -- mzero
  put 83
  return a

runOij3 :: Int -> Maybe Int
runOij3 x = fst $ (runState . runMaybeT $ oij3) x

chain =
  maybe (Validation.Failure 432) (\x -> Validation.Success x) result
  where result = runOij3 13 <|> runOij3 11 <|> runOij3 9

-- qwer :: MomentAlias -> MomentStatus MomentAlias
-- qwer m@(Moment' now pending active result) =
--   -- moveNextPendingToActive m <|> advanceActiveState m <|> Just m
--   maybe (MomentResolved m) MomentPending result
--   where
--     result =
--       addNextGroup `mplus`
--       addNextSound `mplus`
--       removeSound `mplus`
--       removeGroup

advanceActiveState :: MomentAlias -> Maybe MomentAlias
advanceActiveState m@(Moment' now pending active result) = do
  -- guard $ not (null pending && null active)
  nextGroup' <- nextGroup active
  m <- either
    (\_ -> return $ applyDecay' m)
    (\x -> return . replaceGroup nextGroup' $ x)
    (activate now nextGroup')
  return m
  where
    nextGroup =
      head .
      sortBy (comparing group'__resolutionPriority) .
      Set.toList
    applyDecay' m = m { moment'__now = (moment'__now m) + (TimePoint 1000) }
    replaceGroup oldGroup newGroup = m { moment'__active =
                                         Set.insert newGroup
                                         (Set.delete oldGroup active ) }

-- __insertGroup :: EitherT Int (State Int) ()
-- __insertGroup = do

-- TODO: get fns from inside next sound from inside next group; test resolve any state

-- TODO: different actions if pending is empty etc.
-- specify simple return types for all states of Group and Moment (their
-- basic "unfolding" operations are quite similar)

-- nextState'''' m@(Moment' now pending active result) =
--   case (head .
--         sortBy (comparing group'__resolutionPriority) .
--         Set.toList) active of
--     Just group ->
--       case activate now group of
--         Right newGroup -> m { moment'__active =
--                               Set.insert newGroup
--                               (Set.delete group active) }
--         Left oldGroup -> nextState'''' (applyDecay' m)
--     Nothing ->
--       case head pending of
--         Just newGroup -> m { moment'__active =
--                              Set.insert newGroup active }
--         Nothing -> m
--   where
--     applyDecay' m = m { moment'__now = (moment'__now m) + (TimePoint 1000) }

activate now g@(Group' _ pending active result _ _ _) =
  -- TODO: this is the insert action which can be customized by an Event
  if all (canRemove now) active
     then Right $ g {
        group'__pending = drop 1 pending
        , group'__active = Set.fromList $ setStart $ take 1 pending
        , group'__result = result ++ (setStop $ Set.toList active)
        }
     else Left g
  where
    canRemove x = minDurationFilled x
    setStart xs = fmap (\x -> x { event'__start = now }) xs
    setStop xs = fmap (\x -> x { event'__stop = now }) xs
    minDurationFilled now event =
      (start + minDuration) - now <= 0
      where
        getMinDuration       = toTimePoint . event'__minDuration
        (start, minDuration) = (event'__start event, getMinDuration event)

-- asdf =
--   Moment'
--   { moment'__now = TimePoint (13 % 32)
--   , moment'__pending =
--     [ Group'
--       { group'__id = 24
--       , group'__pending =
--         [ Event'
--           { event'__start = TimePoint (0 % 1)
--           , event'__stop = TimePoint (0 % 1)
--           , event'__minDuration = Duration (11 % 8)
--           , event'__maxDuration = Duration (277 % 88)
--           , event'__delta = Duration (18 % 1)
--           , event'__value = 11
--           , event'__getNextState = TestFn1
--           }
--         ]
--       , group'__active = Set.fromList []
--       , group'__result = []
--       , group'__resolutionPriority = 3
--       }
--     , Group'
--       { group'__id = -21
--       , group'__pending =
--         [ Event'
--           { event'__start = TimePoint (0 % 1)
--           , event'__stop = TimePoint (0 % 1)
--           , event'__minDuration = Duration (50 % 29)
--           , event'__maxDuration = Duration (3928 % 899)
--           , event'__delta = Duration (26 % 1)
--           , event'__value = 25
--           , event'__getNextState = TestFn1
--           }
--         ]
--       , group'__active = Set.fromList []
--       , group'__result = []
--       , group'__resolutionPriority = 13
--       }
--     ]
--   , moment'__active = Set.fromList []
--   , moment'__result = []
--   }

  -- nextSound <- head $ Set.toList active -- TODO: sort by resolutionPriority
  -- return nextSound
  -- case fmap isValidGroup nextGroup of
  --   Nothing -> m
  --   Just result ->
  --     m {
  --     moment'__now = now + TimePoint (1%1)
  --     ,
  --     Set.insert result active

-- runMoment' m@(Moment' now gs@(g:_) active result) =
--   |
--   when (gs == [] && active == Set.empty) (left m)
--   if isMember g
--      then nextState'''
--      else bool nextState''' (add g) (isMergeValid g)
--   runMoment'

  -- where
  --   nextGroup = undefined
  --   add = undefined
  --   isMergeValid = undefined

-- runMoment' m@(Moment now groups@(group:_) active result)
--   | isMember group active = nextState''' m
--   | otherwise =
--     bool (insert merged) (nextState''' m) (isValidMoment merged)

  -- case getNextState m of
  --   MomentDone m' -> m'
  --   MomentSuccess m' -> runMoment' m'
  --   GroupFailure m -> runMoment' (applyDecay m)
  --   MomentFailure m -> runMoment' (applyDecay g)

  -- pending <- gets (head . moment'__pending)
  -- when (not (isMember pending)) (insert pending)
  -- nextState <- getNextState


  -- where
  --   insert = undefined

  -- | memberBy group'__id group (moment'__active m) =
  --   let nextGroup = getNextGroup (moment'__active m)
  --       nextActive =
  --         Set.insert (nextGroupState' nextGroup)
  --         (Set.delete nextGroup (moment'__active m))
  --       nextMoment = m { moment'__active = nextActive }
  --   in
  --     bool
  --       (runMoment' groups (resolveMoment m))
  --       nextMoment
  --       (isMomentValid nextMoment)

-- run'' event m@(Moment'' now pending active result) =
--   if memberBy event''__groupId event pending
--      then run''2
--      else m { moment''__pending = Set.insert event pending }

----------------------------------------------------------------------

-- Returns True if all group''__ids in the union of pending and active
-- have one resolved event.
isPotentiallyResolvable = undefined

intersectionBy f a b = Set.intersection (Set.map f a) (Set.map f b)
startsNewPhrase a b = null $ intersectionBy event''__groupId a b

-- any isMatch eventSet
-- where
--   isMatch x = (event''__status x) == Event''Resolved &&
--               (event''__groupId x) == (event''__groupId event)

partitionFirst [] = Nothing
partitionFirst (x:[]) = Just (x, [])
partitionFirst (x:xs) = Just (x, xs)

isPending event pending = memberBy event''__groupId event pending

partitionPending m@(Moment'' _ pending active _) =
  Set.partition (flip isPending pending) active

addNew'' events m@(Moment'' now pending active result) = do
  -- TODO: newtypes for these fields
  guard $ startsNewPhrase events pending
  return m { moment''__pending = Set.union events pending }

sortEvent'' a b
  | event''__resolutionPriority a < event''__resolutionPriority b = GT
  | event''__resolutionPriority a > event''__resolutionPriority b = LT
  | event''__order a < event''__order b = GT
  | event''__order a > event''__order b = LT
  | otherwise = EQ

addPending'' m@(Moment'' now pending active result) = do
  -- currEvent <- head . sortBy sortEvent'' . Set.toList $ pending
  currEvent <- nextToInsert pending
  merged    <- return $ Set.insert currEvent active -- TODO: custom fns, setStart
  guard $ isValidEventSet merged
  guard $ canResolve merged
  return m {
    moment''__pending  = Set.delete currEvent pending
    , moment''__active = merged
    }
  where
    isValidEventSet x = True
    canResolve x      = True

removeActive'' m@(Moment'' now pending active result) = do
  currEvent <- nextToRemove active
  active'   <- return $ Set.delete currEvent active -- TODO: custom fns, set stop, applyDecay
  result'   <- return $ result ++ [currEvent]
  return m {
    moment''__active = active'
    , moment''__result = result'
    }
  where
    sortByPriority = sortBy (comparing event''__resolutionPriority)

getGroups'' :: Set (Event'' a b) -> [[Event'' a b]]
getGroups'' =
  f . Set.toList
  where
    id         = event''__groupId
    group'     = (==) `on` id
    sort'      = sortBy (comparing id)
    f          = List.groupBy group' . sort'

nextEvent'' sortFn xs = do
  candidates <- return
    . catMaybes
    . fmap (head . sortBy (comparing event''__order))
    . getGroups''
    $ xs
  event <- head . sortFn $ candidates
  return event

nextToInsert = nextEvent'' $
               sortBy (comparing event''__resolutionPriority)
nextToRemove = nextEvent'' $
               reverse . sortBy (comparing event''__resolutionPriority)

data OijStatus a = OijFailure | OijSuccess a
  deriving (Eq, Show)

data OijFailure = Done | Pristine | NoPending | NoActive | Diss | CountMax | FalseEq deriving (Eq, Show)

type Oijtype = ([Int], [Int], [Int])
oij4NextPending :: EitherT OijFailure (State Oijtype) ()
oij4NextPending = do
  orig@(pending, active, result) <- get
  next <- return $ head pending
  maybe (left NoPending) (modify . insert) next
  (_, active, result) <- get
  when (sum active > 14) (left Diss)
  -- whenM ((> 14) . sum <$> gets (\(_, x, _) -> x)) (left Diss)
  when (length active > 20) (left CountMax)
  where
    insert x (a, b, c) = (drop 1 a, b ++ [0] ++ [x], c)

oij4NextPending2 orig@(a, b, c) = do
  next@(a', b', c') <- maybe (Left NoPending) (\x -> return $ insert x orig) (head a)
  when (sum b' > 14) (Left Diss)
  when (length b' > 20) (Left CountMax)
  return next
  where
    insert x (a, b, c) = (drop 1 a, b ++ [3] ++ [x], c)

oij4NextActive :: EitherT OijFailure (State Oijtype) ()
oij4NextActive = do
  (_, active, _) <- get
  maybe (left NoActive) (\x -> (modify (\(a, b, c) -> (a, drop 1 b, c ++ [] ++ [x])))) (head active)

oij4NextActive2 (a, b, c) = do
  next <- maybe (Left NoActive) return (head b)
  return (a, drop 1 b, c ++ [next])

oij4DropActive :: EitherT OijFailure (State Oijtype) ()
oij4DropActive = do
  orig@(pending, active, result) <- get
  put ([], [], result ++ [999] ++ pending ++ active)

oijRun s =
  case s of
    (Right (),       s')      -> f oij4NextPending  s'
    (Left NoPending, s')      -> f oij4NextActive   s'
    (Left NoActive,  s')      -> s'
    (Left Diss,      s')      -> f oij4DropActive   s'
    (Left CountMax,  s')      -> f oij4DropActive   s'
  where
    f g x = oijRun $ (runState . runEitherT $ g) x

-- oijRun _ result@([], [], _) = result
-- oijRun f x =
--   case result of
--     (Left NoActive,  _)      -> oijRun oij4DropActive x
--     (Left Diss,      _)      -> oijRun oij4DropActive x
--     (Left CountMax,  _)      -> oijRun oij4DropActive x
--     (Left NoPending, _)      -> oijRun oij4NextActive x
--     (Right (),       result) -> oijRun oij4NextPending result
--   where
--     result = (runState . runEitherT $ f) x

data Moment'' a = Moment'' {
  moment''__now       :: TimePoint
  , moment''__pending :: Set (Event'' Int Event'Params)
  , moment''__active  :: Set (Event'' Int Event'Params)
  , moment''__result  :: [Event'' Int Event'Params]
  } deriving (Eq, Show)

data Group'' a b = Group'' {
  group''__id                   :: a
  , group''__resolutionPriority :: Int
  , group''__events             :: [b]
  } deriving (Ord, Eq, Show)

data Event''Status = Event''Pending | Event''Resolved
  deriving (Ord, Eq, Show)

data Event'' a b = Event'' {
  event''__start                :: TimePoint
  , event''__stop               :: TimePoint
  , event''__minDuration        :: Duration
  , event''__maxDuration        :: Duration
  , event''__delta              :: Duration
  , event''__value              :: a
  , event''__groupId            :: Int
  -- This might seem a bit strange, but it significantly simplifies the code.
  -- Sets instead of sequences resonates better with the idea of the program;
  -- insertion/activation order is already dependent on several other parameters.
  , event''__order              :: Int
  , event''__resolutionPriority :: Int
  -- This might not be necessary any more -- an active event is pending if
  -- `moment''__pending` contains an event with the same groupId.
  , event''__status             :: Event''Status
  , event''__transitionParams   :: b
  } deriving (Ord, Eq, Show)
