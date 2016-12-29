{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sequential2 where

import Protolude
import GHC.Show

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.DList as DL
import Control.Monad.State

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

data Sound6SoundType = Open6 | Close6 | Silent deriving (Ord, Eq, Show)

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
-- TODO: sequence of indices; alternative paths?

data Status =
  Pending { __index :: Int, __value :: Int, __start :: Int, __stop :: Int }
  | Resolved { __index :: Int, __value :: Int, __start :: Int, __stop :: Int}
  deriving (Eq, Show)

keys' = [
  -- [1, 2]
  [1, 2]
  , [2, 1]
  -- , [2, 1]
  -- , [2, 1]
  -- , [1, 2]
  -- , [2, 1]
  -- , [2, 1]
  -- , [1, 2]
  -- , [2, 1]
  -- , [2, 1]
  ]

values' = Map.fromList ([
  (1, [Pending 1 1 0 0, Resolved 1 2 0 0])
  , (2, [Pending 2 1 0 0, Pending 2 2 0 0, Resolved 2 3 0 0])
  ])

data InitState = InitState {
  __seqs :: [[Int]]
  , __events :: Map Int [Status]
  , __curr :: Int
  , __result :: Status
  , __prevSuccess :: Maybe InitState
  } deriving (Eq, Show)

getGroups' :: [Status] -> [[Status]]
getGroups' xs =
  f xs
  where
    id         = __index
    group'     = (==) `on` id
    sort'      = sortBy (comparing id)
    f          = List.groupBy group' . sort'

test limit xs
  | length xs <= 1 = True
  | otherwise = sum (fmap __value xs) <= limit

anyResolved xs = any pred xs
  where pred x = case x of
          Resolved _ _ _ _ -> True
          Pending  _ _ _ _ -> False

allResolved xs = any pred xs
  where pred x = case x of
          Resolved _ _ _ _ -> True
          Pending  _ _ _ _ -> False

-- mergeTest now x curr coll =
--   let groups = getGroups' (curr ++ [x { __start = now }])
--       (resolved, pending) = List.partition anyResolved groups
--   in
--     (concat pending, coll ++ (fmap (\x -> x { __stop = now }) $ concat resolved))

mergeTest now x curr coll =
  let (hasCurrId, notHasCurrId) =
        (List.partition (\y -> __index y == __index x) curr)
      groups = getGroups' notHasCurrId
      (resolved, pending) = List.partition anyResolved groups
  in
    case length (getGroups' (curr ++ [x])) of
      1 -> ([], coll ++ (curr ++ [x]))
      _ ->
        (concat pending ++ (hasCurrId ++ [x]), coll ++ (fmap (\x -> x { __stop = now }) $ concat resolved))

testMerge = mergeTest 0 (Resolved 1 1 0 0) [Pending 1 2 0 0, Resolved 2 1 0 0] []

run2 now [] events (Right ([], coll)) (oldSeqs, oldEvents, oldCurr, oldColl) =
  Right ([], coll)
run2 now [] events (Right (curr, coll)) (oldSeqs, oldEvents, oldCurr, oldColl) =
  run2
    now
    [fmap __index curr]
    events
    (Right (curr, coll))
    (oldSeqs, oldEvents, oldCurr, oldColl)
-- run2 now [] events (Right (curr, coll)) (oldSeqs, oldEvents, oldCurr, oldColl) =
--   Right (curr, coll ++ curr)
run2 now asdf@(x:xs) events (Right (curr, coll)) (oldSeqs, oldEvents, oldCurr, oldColl) =
  let options = zipWith (\a b -> (inner events a b)) x [0..]
      inner events x' index =
        let maybeNew = (getAndRotate x' events)
        in
          case maybeNew of
            Just (currVal, newMap) ->
                -- Do not move on to next step in desired-order list
                -- if the option we are dealing with is not the "desired" one
                  let
                    (mergedCurr, mergedColl) = mergeTest now currVal curr coll
                    newXs = if index == 0
                      then xs
                      else (x:xs)
                    newMap' = if index == 0
                      then newMap
                      else events
                  in
                    -- Important: no value should be invalid in itself
                    case test 60 mergedCurr of
                      True ->
                        run2
                          (now + 1)
                          xs
                          newMap
                          (Right (mergedCurr, mergedColl))
                          (oldSeqs, oldEvents, oldCurr, oldColl)
                      False -> Left (curr, coll)
            Nothing -> panic "asdf"
  in
    case (head $ dropWhile isLeft options) of
      Just x -> x
      Nothing ->
        let (b, c) = case (head oldSeqs, drop 1 oldSeqs) of
              (Nothing,  _) -> panic (Protolude.show oldSeqs)
              (Just [], c') -> panic (Protolude.show coll)
              (Just b', c') -> (drop 1 b', c')
        in
          run2 now (b:c) oldEvents (Right (oldCurr, oldColl)) ((b:c), oldEvents, oldCurr, oldColl)

testResult =
  case (run2 0 keys' values' (Right ([], [])) (keys', values', [], [])) of
    Right result -> fmap (\x -> (__index x, __value x, __start x, __stop x)) (snd result)
