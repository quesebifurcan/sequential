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
import Test.HUnit

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

instance Arbitrary Pitch
  where arbitrary = do
          pitchRatio <- genLimitedRatio
          octave     <- choose (1, 10)
          return $ Pitch (PitchRatio pitchRatio) (Octave octave)

-- instance Arbitrary MergeStrategy
--   where arbitrary = do
--           return $ MergeStrategy (\sound moment -> Set.insert sound moment)

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
  resolutionStatus <- return $ slope phraseLength Pending Resolved
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
        keyCount    <- choose (20, 40)
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
          minDuration      <- genPosRatio
          maxDuration      <- genPosRatio
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
          events <- arbitrary :: Gen (Events Text Sound)
          return $ Moment
              (TimePoint (0 % 1))
              events
              Set.empty
              []

iterateN n f = foldr (.) identity (replicate n f)

genMomentRandomState :: Gen Moment
genMomentRandomState = do
  moment <- arbitrary :: Gen Moment
  n      <- choose (0, (length . events__keys . moment__events) moment)
  return $ iterateN n f moment
  where f m@(Moment _ events _ result) =
          case getNextEvent events of
            Just x  -> m {
              moment__events = x
              , moment__result = result ++ [soundDefault]
              }
            Nothing -> m

-- prop_asdf =
--   forAll genMomentRandomState prop_getNextEvent

prop_getNextEvent :: Moment -> Property
prop_getNextEvent m@(Moment _ events _ _) =
  case getNextEvent events of
    Nothing     -> property $ (events__keys events) == []
    Just result -> property $ (events__curr events) /= (events__curr result)

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

-- prop_forceResolve :: Moment -> Property
-- prop_forceResolve1 moment =
--   property $
--   (moment__active $ snd $ runState forceResolve moment)
--   ==
--   Set.empty

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

prop_insertSound :: Moment -> Property
prop_insertSound m@(Moment now events active result) =
  property $
  not (Set.member (events__curr events) (moment__active m))
  &&
  Set.member (events__curr events) (moment__active . insertSound $ m)

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

main = do
  runTests
