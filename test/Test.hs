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

import Test.QuickCheck

instance Arbitrary PitchRatio
  where arbitrary = do
          n <- choose (1, 100)
          d <- choose (1, 100)
          return $ PitchRatio (n % d)
--   where arbitrary = elements [Open6, Close6]

-- prop_getGroups3 :: Map Int Sound6 -> Property

prop_getGroups3 :: PitchRatio -> Property
prop_getGroups3 pitchRatio =
  pitchRatio === PitchRatio (3 % 4)
  -- (sum (fmap (length . Map.elems) $ getGroups sounds)) ===
  -- (length (Map.elems sounds))

return []
runTests = $quickCheckAll

main = do
  runTests
