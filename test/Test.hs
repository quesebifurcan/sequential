{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Protolude

import Sequential4

import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Test.HUnit
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Error.Class as Error
import Control.Monad.Trans.Either
import Control.Monad.Loops (whileM_, untilM_)

import qualified Text.Show.Pretty as PP

import Test.QuickCheck

-- generate (arbitrary :: Gen PitchRatio)
-- sample genLimitedRatio
-- generate (arbitrary :: Gen Sound)

genPosRatio :: Gen (Ratio Integer)
genPosRatio = do
  n <- choose (1, 100)
  d <- choose (1, 100)
  return (n % d)

genLimitedRatio :: Gen (Ratio Integer)
genLimitedRatio = fmap limitRatio genPosRatio

instance Arbitrary PitchRatio
  where arbitrary = do
          r <- genLimitedRatio
          return $ PitchRatio r

instance Arbitrary MomentConstraint
  where arbitrary = do
          size'           <- choose (1, 5)
          dissonanceLimit <- vectorOf size' (arbitrary :: Gen PitchRatio)
          maxCount        <- choose (1, 10)
          return $ MomentConstraint (Set.fromList dissonanceLimit) maxCount

momentConstraintBypass =
  MomentConstraint pitchRatios maxCount
  where
    pitchRatios =
      (Set.fromList [PitchRatio (1 % 1) , PitchRatio (100000 % 99999)])
    maxCount = 10000000000

prop_testBypassMomentConstraint :: Set Sound -> Property
prop_testBypassMomentConstraint sounds =
  property $ isConsonant limit sounds
  where
    limit = momentConstraint__dissonanceLimit momentConstraintBypass

-- testMomentState :: State Moment ()
-- testMomentState = do
--   modify insertSound
--   gets moment__active >>= return . removeSounds >>= modify
--   whenM ((/= []) <$> gets (events__keys . moment__events)) $ do
--     gets moment__events >>= return . getNext >>= modify
--     testMomentState
--   where
--     getNext x = (\m -> m { moment__events = getNextEvent x })

-- prop_testInsertSound :: Moment -> Property
-- prop_testInsertSound moment =
--   length (moment__result result) === (length keys + 1)
--   -- length (moment__result result) === 1
--   where
--     result = snd $ runState testMomentState moment
--     active = moment__active result
--     keys   = (events__keys . moment__events) moment

instance Arbitrary (Group Int Sound)
  where arbitrary = do
          id <- arbitrary :: Gen Int
          -- sounds <- listOf1 (arbitrary :: Gen Sound)
          pending <- arbitrary :: Gen [Sound]
          active <- arbitrary :: Gen (Set Sound)
          resolutionPriority <- arbitrary :: Gen Int
          return $ Group id pending active [] resolutionPriority

instance Arbitrary Pitch
  where arbitrary = do
          pitchRatio <- genLimitedRatio
          octave     <- choose (1, 10)
          return $ Pitch (PitchRatio pitchRatio) (Octave octave)

-- instance Arbitrary MergeStrategy
--   where arbitrary = do
--           return $ MergeStrategy (\sound moment -> Set.insert sound moment)

genGroup' :: TimePoint -> Gen (Group Int Sound)
genGroup' timePoint = do
  g@(Group id pending active result _) <- arbitrary :: Gen (Group Int Sound)
  return $
    g { group__active = Set.map setStart active }
  where
    setStart x = x { sound__start = timePoint }

genGroups' :: Gen [(Group Int Sound)]
genGroups' = do
  count <- choose (1, 10)
  timePoints <- vectorOf count (arbitrary :: Gen TimePoint)
  groups <- mapM genGroup' (sort timePoints)
  return groups

prop_leastRecent :: Property
prop_leastRecent =
  forAll genGroups' test
  where
    test :: [(Group Int Sound)] -> Property
    test groups =
      case (head $ filter hasActive groups
           , getLeastRecentlyUpdated groups) of
        (Just x, Just y) -> x === y
        (Nothing, y) -> y === Nothing

prop_nextGroupState :: TimePoint -> Group Int Sound -> Property
prop_nextGroupState now g@(Group id pending active result _) =
  case (head pending, nextState) of
    (Nothing, Left group) -> group === g
    (Just sound, Right group) ->
      Set.member now (Set.map sound__start . group__active $ group) .&&.
      (length (group__pending g) - length (group__pending group)) === 1
    _ -> property False
  where
    nextState = nextGroupState now g

incrementalSum :: [Int] -> [Int]
incrementalSum =
  snd . foldl' f (0, [])
  where
    f (curr, coll) x = (curr + x, coll ++ [curr + x])

slope count start end =
  (take (count - 1) . repeat $ start) ++ [end]

genGroup :: Int -> Gen [Sound]
genGroup groupId = do
  phraseLength     <- choose (1, 10)
  soundIds         <- fmap incrementalSum $ replicateM phraseLength (choose (0, 1))
  resolutionStatus <- return $ slope 1 Resolved Resolved
  sounds           <- replicateM phraseLength (arbitrary :: Gen Sound)
  -- groupId          <- arbitrary :: Gen Int
  return $
    getZipList $
    (\soundId status sound ->
       sound {
        sound__horizontalGroup = groupId
        , sound__verticalGroup = soundId
        , sound__status = status
        }) <$>
    ZipList soundIds <*>
    ZipList resolutionStatus <*>
    ZipList sounds

-- rm :: State Moment ()
-- rm = do
--   -- modify removeAllRemovable
--   -- curr <- get
--   modify removeAllRemovable
--   m@(Moment now events active result) <- get
--   if active == Set.empty
--      then return ()
--      else (do (modify applyDecay)
--               rm)

-- prop_removeAllRemovable =
--   forAll genMomentRandomState f

  -- where f m@(Moment now events active result) =
  --         (moment__active $ snd $ runState rm m) == Set.empty
  --         -- property $ (moment__active . rm) m === Set.empty

genPhrase :: Text -> Gen (Map Text [Sound])
genPhrase label = do
  count  <- choose (1, 10)
  groups <- mapM genGroup [0..count]
  return $ Map.fromList [(label, concat groups)]

genActive :: Gen (Set Sound)
genActive = do
  count  <- choose (1, 10)
  groups <- mapM genGroup [0..count]
  return . Set.fromList . concat $ groups

instance Arbitrary (Events Text Sound)
  where arbitrary = do
        phraseCount <- choose (1, 7)
        phrases     <- mapM genPhrase (fmap show [0..(phraseCount :: Int)])
        keyCount    <- choose (2000, 4000)
        -- N.B. currently invalid -- in order for `keys` to result in a valid
        -- seq (which can be used by e.g. getNextEvent'), it needs to
        -- match the counts in phrases (number of sounds in phrase). This
        -- is problematic -- use a different data structure?
        keys        <- replicateM keyCount (elements (fmap (\x -> show x :: Text) [0..(phraseCount :: Int)]))
        case mkEvents keys (Map.unions phrases) of
          Just result  -> return result
          Nothing      -> panic "Error in quickcheck generator; `mkEvents` should never return Nothing"

instance Arbitrary Sound
  where arbitrary = do
          pitch            <- arbitrary :: Gen Pitch
          velocity         <- genLimitedRatio
          start            <- genPosRatio
          -- TODO: `stop` should be greater than `start` + `minDuration`
          stop             <- genPosRatio
          minDuration      <- return (1 % 1)
          maxDuration      <- return (2 % 1)
          deltaDuration    <- genPosRatio
          envelope         <- elements [Impulse, Sustained]
          constraint       <- arbitrary :: Gen MomentConstraint
          instrument       <- elements ["a", "b", "c", "d"]
          -- Create generator for phrases as well, using Pending/Resolved
          resolutionStatus <- elements [Resolved]
          verticalGroup    <- arbitrary :: Gen Int
          horizontalGroup  <- arbitrary :: Gen Int
          return $ Sound {
            sound__pitch             = pitch
            , sound__velocity        = Velocity velocity
            , sound__start           = TimePoint start
            , sound__stop            = TimePoint stop
            , sound__minDuration     = Duration minDuration
            , sound__maxDuration     = Duration maxDuration
            , sound__deltaDuration   = Duration deltaDuration
            , sound__envelope        = envelope
            , sound__instrument      = Instrument instrument
            , sound__horizontalGroup = horizontalGroup
            , sound__verticalGroup   = verticalGroup
            , sound__status          = resolutionStatus
            , sound__constraint      = constraint
            }

prop_limitRatio =
  forAll genPosRatio (property . isValid . limitRatio)
  where isValid x = x >= 1 && x < 2

instance Arbitrary Moment
  where arbitrary = do
          events <- arbitrary :: Gen [Group Int Sound]
          return $ Moment
              (TimePoint (0 % 1))
              events
              Set.empty
              []

iterateN n f = foldr (.) identity (replicate n f)

-- genMomentRandomState :: Gen Moment
-- genMomentRandomState = do
--   moment <- arbitrary :: Gen Moment
--   n      <- choose (1, (length . events__keys . moment__events $ moment) - 0)
--   return $
--     execState (insertN n) moment

-- insertN :: Int -> State Moment ()
-- insertN n = do
--   replicateM_ n $ do
--     modify insertSound
--     gets moment__events >>= return . getNext >>= modify
--   where
--     getNext x = (\m -> m { moment__events = getNextEvent x })

-- prop_asdf =
--   forAll genMomentRandomState prop_getNextEvent

-- prop_getNextEvent :: Moment -> Property
-- prop_getNextEvent m@(Moment _ events _ _) =
--   property $ (events__curr events) /= (events__curr result)
--   where result = getNextEvent events

prop_getGroups :: Set Sound -> Property
prop_getGroups sounds =
  property $
  length groups ==
  Set.size (Set.map sound__horizontalGroup sounds)
  where
    groups = getGroups sounds

instance Arbitrary TimePoint
  where arbitrary = do
          r <- genPosRatio
          return $ TimePoint r

prop_canRemove :: TimePoint -> Set Sound -> Property
prop_canRemove now sounds =
  property $
  all (\sound -> minDurationFilled now sound &&
                 isSoundResolved sound sounds)
  (filterCanRemove now sounds)

-- TODO: can a different kind of ordering be used? Objective: avoid fragile,
-- , difficult-to-validate logic in constructor functions + ambiguity
-- in the "cycle" machinery.
-- Groups: insert group if not already present.
-- top-level seq: [Group] (instead of Events)
-- for insertion: 1. delete all curr elts in group 2. keep all elts in group

-- prop_getNextEvent' :: Property
-- prop_getNextEvent' =
--   forAll genMomentRandomState f
--   where f m@(Moment now events active result) =
--           (not (null active)) ==>
--           Set.member
--           (sound__horizontalGroup next)
--           (Set.map sound__horizontalGroup active)
--           where
--             next =
--               case events__curr events == (events__curr $ getNextEvent' active events) of
--                 True -> panic "oijsdf"
--                 False -> events__curr $ getNextEvent' active events

-- prop_removeAllRemovable

-- prop_forceResolve1 :: Property
-- prop_forceResolve1 =
--   forAll genMomentRandomState f
--   where
--     f m@(Moment now events active result) =
--       (not (null active)) ==>
--       (moment__active $ snd $ runState forceResolve m)
--       ===
--       Set.empty

-- prop_applyDecay =
--   forAll genMomentRandomState f
--   where
--     f m@(Moment now events active result) =
--       (not (null active)) ==>
--       (moment__now $ applyDecay m) > now

-- prop_forceResolve2 moment =
--   property $
--   (moment__result $ snd $ runState forceResolve moment)
--   /=
--   []

-- prop_getGroupOfSound :: Set Sound -> Property
-- prop_getGroupOfSound x xs =
--   property $
--   (== 1) $
--   Set.size $
--   Set.map sound__verticalGroup (getGroupOfSound x (Set.insert x xs))

-- prop_insertSound :: Moment -> Property
-- prop_insertSound m@(Moment now events active result) =
--   -- property $
--   -- not (Set.member (events__curr events) (moment__active m))
--   -- &&
--   -- Set.member (events__curr events) (moment__active . insertSound $ m)
--   -- &&
--   let newState = insertSound (m { moment__now = TimePoint (13 % 1) })
--   in
--     case head (moment__active newState) of
--       Just x -> sound__start x === TimePoint (13%1)
--       Nothing -> property True

  -- all (\x -> sound__start x == now) (moment__active . insertSound $ m)

-- genSizedPitchRatioSet :: Gen (Set PitchRatio)
-- genSizedPitchRatioSet = do
--   size <- choose (3, 20)
--   ratios <- vectorOf size (arbitrary :: Gen PitchRatio)
--   return $ Set.fromList ratios

-- prop_dissonanceScore_1 :: (Set PitchRatio) -> Property
-- prop_dissonanceScore_1 pitchRatios =
--   let sorted =
--         sortBy
--         (comparing (\(PitchRatio r) -> harmonicDistance r))
--         (Set.toList pitchRatios)
--   in
--     property $
--     dissonanceScore (drop 1 sorted) >=
--     dissonanceScore (drop 1 . reverse $ sorted)

-- data PitchRatioScore = PitchRatioScore {
--   pitchRatioScore__score :: Int
--   , pitchRatioScore__ratio :: PitchRatio
--   } deriving Show

-- instance Arbitrary PitchRatioScore
--   where arbitrary = do
--           (score, ratio) <- elements [
--             (0, PitchRatio (1 % 1))
--             , (1, PitchRatio (3 % 2))
--             , (2, PitchRatio (5 % 4))
--             , (3, PitchRatio (7 % 4))
--             , (4, PitchRatio (9 % 8))
--             , (5, PitchRatio (11 % 8))
--             ]
--           return $ PitchRatioScore score ratio

-- prop_dissonanceScore_2 :: [PitchRatioScore] -> [PitchRatioScore] -> Property
-- prop_dissonanceScore_2 xs ys =
--   let xsScore = sum $ fmap pitchRatioScore__score xs
--       ysScore = sum $ fmap pitchRatioScore__score ys
--       xsRatios = fmap pitchRatioScore__ratio xs
--       ysRatios = fmap pitchRatioScore__ratio ys
--       xs_ = (dissonanceScore xsRatios)
--       ys_ = (dissonanceScore ysRatios)
--   in
--     case xs_ > ys_ of
--       True -> property $ xsScore > ysScore
--       False -> property True

-- prop_dissonanceScore_2 pitchRatios =
--   property $
--   dissonanceScore pitchRatios >=
--   dissonanceScore (drop 1 pitchRatios)

-- prop_harmonicDistance x =
--   forAll genLimitedRatio
--   where a b =

-- prop_harmonicDistance
-- prop_getGroups3 :: PitchRatio -> Property
-- prop_getGroups3 pitchRatio =
--   pitchRatio === PitchRatio (3 % 4)
--   -- (sum (fmap (length . Map.elems) $ getGroups sounds)) ===
--   -- (length (Map.elems sounds))

printSampleSound = fmap PP.ppDoc (generate (arbitrary :: Gen Sound))
printSampleSounds = fmap PP.ppDoc (sample' (arbitrary :: Gen Sound))

return []
runTests = $quickCheckAll

instance Arbitrary MomentAlias
  where arbitrary = do
          now <- genPosRatio
          pendingCount <- choose (0, 5)
          pending <- vectorOf pendingCount (arbitrary :: Gen GroupAlias)
          active <- vectorOf pendingCount (arbitrary :: Gen GroupAlias)
          return $ Moment' (TimePoint now) pending (Set.fromList active) []

instance Arbitrary GroupAlias
  where arbitrary = do
          id <- arbitrary :: Gen Int
          pendingCount <- choose (0, 5)
          pending <- vectorOf pendingCount (arbitrary :: Gen EventAlias)
          activeCount <- choose (0, 5)
          active <- vectorOf activeCount (arbitrary :: Gen EventAlias)
          priority <- arbitrary :: Gen Int
          return $
            Group'
            id pending (Set.fromList active) []
            priority GroupSustain GroupRemoveAll

prop_removeEvent :: MomentAlias -> Property
prop_removeEvent moment =
  let activeEventsCount = Set.size . getEvents
      getEvents = Set.unions . fmap group'__active . Set.toList . moment'__active
      removeEvent m = fst $ (runState . runMaybeT $ removeEvent2) m
  in
    case removeEvent moment of
      Just result ->
        ((activeEventsCount moment) - (activeEventsCount result) === 1) .&&.
        (all (minDurationFilled' (moment'__now result))
         (concat . fmap group'__result . Set.toList . moment'__active $ result))
      Nothing -> getEvents moment === Set.empty

prop_removeGroup :: MomentAlias -> Property
prop_removeGroup moment =
  let activeGroupsCount = length . getEmptyGroups
      getEmptyGroups = filter isEmptyGroup . Set.toList . moment'__active
  in
    case removeGroup moment of
      Just result -> (activeGroupsCount moment) - (activeGroupsCount result) === 1
      Nothing -> (activeGroupsCount moment) === 0

prop_addGroup :: MomentAlias -> Property
prop_addGroup moment =
  case addGroup moment of
    Just result ->
      (Set.size . moment'__active $ result) -
      (Set.size . moment'__active $ moment) === 1
    Nothing -> case head . moment'__pending $ moment of
      Just x -> property $ memberBy group'__id x (moment'__active moment)
      Nothing -> property True

instance Arbitrary EventAlias
  where arbitrary = do
          minDuration <- genPosRatio
          maxDuration <- genPosRatio
          deltaN <- choose (0, 100)
          value <- arbitrary :: Gen Int
          fn <- elements [TestFn1]
          return $
            Event'
            (TimePoint 0)
            (TimePoint 0)
            (Duration minDuration)
            (Duration (minDuration + maxDuration))
            (Duration (deltaN % 1))
            (abs value)
            fn

main = do
  runTests
