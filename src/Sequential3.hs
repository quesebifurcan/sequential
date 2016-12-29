{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sequential3 where

import Protolude
import GHC.Show

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.DList as DL
import qualified Test.HUnit as HUnit
import qualified Test.QuickCheck as QC
import qualified Control.Arrow as Arrow
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Error.Class as Error

---------------------------------------------
-- Flatter data structure -- using sets
-- Add Silence as a data type?

-- Using Map?
-- 1. Like sets, it can be guaranteed that only one elem with id x is present in coll
-- 2. More flexible lookup functions; still access to e.g. `difference`:
--   Map.difference old new -> get removed elems after application of constraints

-- TODO: Variable instrumentation: output score in a format which can
-- be interpreted by multiple synth setups

-- Use getAndRotate applied to a list of iterators?

harmonicDistance n d =
  logBase 2 . fromRational . limit $ max n d % min n d
  where limit n
          | n < 1     = limit (n * 2)
          | n >= 2    = limit (n / 2)
          | otherwise = n

data Sound6SoundType = Open6 | Close6 | Silent
  deriving (Ord, Eq, Show)

newtype SoundId6 = SoundId6 Int deriving (Ord, Eq, Show)

data Sound6 = Sound6 {
  sound6__group_id   :: Int
  , sound6__sound_id :: Int
  , sound6__pitch    :: Int
  , sound6__type     :: Sound6SoundType
  } deriving (Ord, Eq, Show)

insertSound6 sound m = Map.insertLookupWithKey f id sound m
  where f _ newValue _ = newValue
        id             = sound6__sound_id sound

insertWithReset sound m = (Just m, Map.singleton sound)

insertSound6_2 sound = Map.insert (sound6__sound_id sound) sound
-- insertSounds = foldl' (\m sound -> insertSound6_2 sound m) Map.empty

state6_1 = foldl' (\m sound -> insertSound6_2 sound m) Map.empty [
  Sound6 0 2 0 Open6
  , Sound6 0 3 2 Open6
  , Sound6 0 4 3 Open6
  , Sound6 1 5 4 Open6
  ]

isResolved = any pred . Map.elems
  where pred = (== Close6) . sound6__type

getGroups :: Ord k => Map k Sound6 -> [Map k Sound6]
getGroups =
  fmap Map.fromList . f . Map.toList
  where
    id         = sound6__group_id . snd
    group'     = (==) `on` id
    sort'      = sortBy (comparing id)
    f          = List.groupBy group' . sort'

stateGroups6 =
  let groupsBySoundId =
        List.groupBy
        ((==) `on` sound6__group_id)
        (sortBy (comparing sound6__group_id)
         (Map.elems state6_1))
  in
    -- To get e.g. lowest pitch in each group
    fmap (sound6__pitch . minimumBy (comparing sound6__pitch)) groupsBySoundId

------------------------------------------
-- iterate

rotate' n xs = zipWith const (drop n (cycle xs)) xs

getAndRotate k m = do
  currSeq <- Map.lookup k m
  currVal <- head currSeq
  newMap  <- return $
    Map.insert k (rotate' 1 currSeq) m
  return (currVal, newMap)

------------------------------------------
-- Resolve pending

getPending :: Map Int Sound6 -> Map Int Sound6
getPending = Map.filter isPending
  where isPending = (==) Open6 . sound6__type

instance QC.Arbitrary Sound6SoundType
  where arbitrary = QC.elements [Open6, Close6]

instance QC.Arbitrary Sound6
  where arbitrary = do
          type_ <- (QC.arbitrary :: QC.Gen Sound6SoundType)
          soundId <- QC.choose (0, 15)
          groupId <- QC.choose (0, 5)
          return $ Sound6 groupId soundId 0 type_

genGroupIds :: QC.Gen [Int]
genGroupIds =
  QC.sized $
  (\s ->
     QC.vectorOf
     (min s 10)
     (QC.choose (0, 5)))

genSeqs :: QC.Gen ([Int], (Map Int [Sound6]))
genSeqs = do
  maps <- QC.suchThat soundMaps ((/=) Map.empty)
  seqs <- keySeq maps
  return (seqs, maps)
  where soundMaps = QC.arbitrary :: QC.Gen (Map Int [Sound6])
        keySeq    = QC.listOf . QC.elements . Map.keys

prop_getGroups1 :: Map Int Sound6 -> QC.Property
prop_getGroups1 sounds =
  (sum (fmap (length . Map.elems) $ getGroups sounds)) QC.===
  (length (Map.elems sounds))

prop_getGroups2 :: Map Int Sound6 -> Bool
prop_getGroups2 sounds =
  all (== 1) keyCounts
  where
    groups = getGroups sounds
    keyCount =
      length
      . Set.fromList
      . fmap sound6__group_id
      . Map.elems
    keyCounts = fmap keyCount groups

prop_insertSound :: Map Int Sound6 -> Sound6 -> Bool
prop_insertSound m x = undefined

state6_2 = Set.fromList [
  Sound6 0 2 0 Open6
  , Sound6 0 3 2 Open6
  , Sound6 0 4 3 Open6
  , Sound6 1 5 4 Close6
  ]

-- TODO: use Set; it feels so much more appropriate.
-- insertLookupWithKey-functionality can easily be replaced by Set.partition. Keep all Ord instances default.
filterMoment now pred (active, result) =
  let (remove, keep) = Set.partition pred active
  in
    (keep,
     result ++ (fmap (\x -> x { sound6__pitch = now }) (Set.toList remove)))

  -- let (thisGroup, otherGroups) = partitionBy sound6__group_id x xs
  --     (toRemove, toKeep) = partitionBy sound6__sound_id x thisGroup
  --     partitionBy f x          = Set.partition $ (==) (f x) . f
  -- in
  --   (toRemove, Set.unions [otherGroups, toKeep, Set.singleton x])

insertIntoGroup :: Sound6 -> Set Sound6 -> (Set Sound6, Set Sound6)
insertIntoGroup x xs =
  (toRemove, Set.union toKeep toAdd)
  where (toRemove, toKeep) = Set.partition f xs
        toAdd              = Set.singleton x
        f                  = (==) (sound6__id x) . sound6__id

isResolved' (Sound6 _ _ _ Close6) = True
isResolved' (Sound6 _ _ _ _)      = False

-- anyResolved :: Foldable t => t a -> Bool
anyResolved :: Foldable t => t Sound6 -> Bool
anyResolved xs = any isResolved' xs

getGroups' :: Set Sound6 -> [Set Sound6]
getGroups' =
  fmap Set.fromList . f . Set.toList
  where
    id         = sound6__group_id
    group'     = (==) `on` id
    sort'      = sortBy (comparing id)
    f          = List.groupBy group' . sort'

dissonanceScore = sum . Set.map sound6__pitch
isConsonant = (< 5) . dissonanceScore

mapTuple f = join (Arrow.***) f

removeResolvedGroups :: Set Sound6 -> (Set Sound6, Set Sound6)
removeResolvedGroups =
  fmap (mapTuple Set.unions) $
  List.partition anyResolved . getGroups'

sound6__id = sound6__sound_id Arrow.&&& sound6__group_id
nubSound6 = Set.fromList . List.nubBy f . Set.toList
  where f  = (==) `on` sound6__id

-- QC.quickCheckWith QC.stdArgs { QC.maxSuccess = 500 } prop_insertIntoGroup

-- check that toKeep contains added elt
-- (True QC.=== True) QC..&&. (False QC.=== True) :: QC.Property

prop_insertIntoGroup1 :: Sound6 -> Set Sound6 -> QC.Property
prop_insertIntoGroup1 x xs
  | isMember =
    QC.counterexample "One element is removed"
    $ Set.size toRemove QC.=== 1
    QC..&&.
    Set.member x toKeep
  | otherwise =
    QC.counterexample "No element is removed"
    $ Set.size toRemove QC.=== 0 QC..&&. Set.member x toKeep
  where
    xs'                = nubSound6 xs
    isMember           = Set.member (id x) (Set.map id xs')
    (toRemove, toKeep) = insertIntoGroup x xs'
    id                 = sound6__id

-- prop_removeResolved :: Set Sound6 -> QC.Property
-- prop_removeResolved xs =
--   QC.counterexample "No element is dropped"
--   (Set.union (Set.unions a) (Set.unions b) QC.=== xs)
--   QC..&&.
--   QC.counterexample "All resolved groups are collected"
--   (all anyResolved a)
--   QC..&&.
--   QC.counterexample "All unresolved groups are collected"
--   (not (all anyResolved b) || b == [])
--   where (a, b) = removeResolvedGroups xs

allResolved :: Foldable t => t Sound6 -> Bool
allResolved = all isResolved'

-- -- tryResolve [] m (Right (curr, result)) = Right (curr, result ++ [curr])

-- tryResolve (k:ks) m (Right (curr, result))
--   | not (Set.member k curr) = (Set.empty, [])
--   | otherwise = inner nextEvent

--   where
--     nextEvent = getAndRotate k m
--     (a, b) = nextEvent
--     (c, d) = insertIntoGroup

tryResolve ::
  [Int]
  -> Map Int [Sound6]
  -> Either Text (Set Sound6, [Sound6])
  -> Maybe (Set Sound6)
tryResolve (k:ks) m (Right (curr, result))  = do
  guard (Set.member k (Set.map sound6__group_id curr))
  nextState <- return $ getAndRotate k m
  guard (isJust nextState)
  (nextEvent, nextMap) <- nextState
  (a, b) <- return $ insertIntoGroup nextEvent curr
  return b

-- tryResolve2 ::
--   [Int]
--   -> Map Int [Sound6]
--   -> Either (Set Sound6, [Set Sound6]) (Set Sound6, [Set Sound6])
--   -> Either (Set Sound6, [Set Sound6]) (Set Sound6, [Set Sound6])

-- Using custom state:

data MyState = S deriving (Eq, Show)

type MyMonadT e m a = StateT Int (ExceptT e m) a

runMyMonadT :: (Monad m) => MyMonadT e m a -> Int -> m (Either e a)
runMyMonadT m = runExceptT . evalStateT m

type MyMonad e a = MyMonadT e Identity a
runMyMonad m = runIdentity . runMyMonadT m

testing :: MyMonad Int Int
testing = do
  a <- get
  value <- maybe (Error.throwError a) return (Map.lookup a (Map.fromList [(987, 30)]))
  return (a+value)

-- Simple use of Except:
-- run: testing2 987 2 -> ExceptT (Identity (Right 1063)
-- OR: runIdentity . runExceptT $ testing2 987 2 -> (Right 1063)

testing2 :: Int -> Int -> [Int] -> Except Text Int
testing2 x y zs = do
  asdf      <- maybe (Error.throwError "maybe error") return (f x)
  qwer      <- bool (Error.throwError "bool error") (return $ asdf + 1000) (2 == y)
  oij       <- either (return $ Error.throwError "either error") return (Right 100000)
  maybeHead <- return $ head [1,2]
  maybe (Error.throwError "no head") return maybeHead
  iuy       <- return $ (do nextIndex <- maybeHead
                            nextEvent <- Map.lookup nextIndex (Map.fromList [(1, 80)])
                            return nextEvent)
  -- -------------- Alternative iuy: -----------------
  -- iuy  <- return $
  --   join $
  --   Map.lookup <$>
  --   (head zs) <*>
  --   pure (Map.fromList [(30, 8000)])
  zxcv <- maybe
    (Error.throwError "Empty list for argument `zs`")
    return
    iuy
  return $ 3 + asdf + qwer + oij + zxcv

  where f x = Map.lookup x (Map.fromList [(987, 30)])

data Err1 a b = Err1_1 a | Err_1_2 b deriving (Eq, Show)

testing3 :: ExceptT Text Maybe Int
-- testing3 :: MaybeT (Except Text) Int
-- testing3 :: Maybe Int
testing3 = do
  nextIndex <- lift $ head [1,2,3]
  nextEvent <- lift $ Map.lookup nextIndex (Map.fromList [(1, 80)])
  -- return nextEvent
  unless (nextEvent > 9) (Error.throwError "oijsdf")
  -- guard False
  return nextEvent



  -- value <- maybe (Error.throwError a) return (Map.lookup a (Map.fromList [(987, 30)]))
  -- value2 <- maybe (Error.throwError 765234) return (Map.lookup x (Map.fromList [(1, 20)]))
  -- -- unless (a > 8) (Error.throwError 1234)
  -- -- if (a > 20)
  -- --   then (do
  -- --            modify (\x -> x - 1)
  -- --            testing
  -- --        )
  -- --   else (do
  -- --            modify (+100)
  -- --            get
  -- --        )
  -- -- a <- get
  -- return (value + value2)


-- Recursion with Either monad
tryResolve2 :: Either (Set Int) (Set Int) -> Either (Set Int) (Set Int)
tryResolve2 x' = do
  -- asdf <- return $ Map.lookup 9 (Map.fromList [(1,2), (2,3)])
  x <- x'
  unless (isRight x')     (Error.throwError x)
  unless (Set.member 2 x) (Error.throwError x)
  unless (Set.member 8 x) (panic "oijsdf")
  -- unless (isJust asdf) (Error.throwError x)
  -- result <- case (x == Set.fromList [2]) of
  --   True -> tryResolve2 (Set.insert 987 x)
  --   False -> return x
  result <-
    bool
    (return x)
    (tryResolve2 $ return (Set.insert 987 x))
    (x == Set.fromList [2])
  return result



genTest :: QC.Gen (Int, Int)
genTest = do
  a <- QC.arbitrary :: QC.Gen Int
  return (a, a)

prop_testGen x = QC.forAll genTest (\(a, b) -> a == b)

prop_tryResolve2 :: QC.Property
prop_tryResolve2 =
  QC.forAll genSeqs (\xs -> Map.keys (snd xs) QC.=== [1,2,3])


-- TODO: use actual "sounds"

-- tryResolve [1,2,3] (Map.fromList [(1,[SSound6 0 0 0 Open6])]) (Right (Set.fromList [], []))

  -- case 8 of
    -- Just (v, m')  ->
    --   tryResolve
    --   ks
    --   m'
    --   (Right (Set.union toRemove unresolved, result))
    --   where
    --     (toRemove, toKeep)     = insertIntoGroup v curr
    --     (resolved, unresolved) = removeResolvedGroups toKeep

    -- Nothing           -> Left (curr, result)
    -- _ -> panic "oijsdf"
    -- (Just k, True) ->
    -- (Just k, False) ->
    -- (Nothing, True) ->
    -- (Nothing, False) ->
    -- case Set.member k curr of
    --   True ->
    --     case getAndRotate k m of
    --       Just (nextInsert, m') ->
    --         case insertIntoGroup nextInsert curr of

isConsonant_ s = undefined

-- memberTest =
--   (Set.member
--    (sound6__sound_id x)
--    (Set.map sound6__sound_id xs)) &&
--   (Set.member
--    (sound6__group_id x)
--    (Set.map sound6__group_id xs))
--   where x = Sound6 {sound6__group_id = 2, sound6__sound_id = 8, sound6__pitch = 0, sound6__type = Close6}
--         xs = Set.fromList [Sound6 {sound6__group_id = 1, sound6__sound_id = 8, sound6__pitch = 0, sound6__type = Open6},Sound6 {sound6__group_id = 2, sound6__sound_id = 2, sound6__pitch = 0, sound6__type = Open6}]

-- props:
-- 1. same element count,
-- 2. all ids in remove should be the same as x
-- 3. if group id of x is not in xs, remove should be empty

-- prop_filterMoment Int -> Map

  -- if group id is already in m, the same number of elements
  --   old elem should have same group id as new
  -- otherwise, one more element

  -- testInsert: sound id is replaced; old returned
  --           : add to group
  --           : reset group, add
