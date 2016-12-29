{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Protolude

import Sequential3

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.QuickCheck

prop_getGroups3 :: Map Int Sound6 -> Property
prop_getGroups3 sounds =
  (sum (fmap (length . Map.elems) $ getGroups sounds)) ===
  (length (Map.elems sounds))

return []
runTests = $quickCheckAll

main = do
  runTests
