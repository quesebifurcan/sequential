{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude

data Note = Note {
  pitch :: Int
  , duration :: Int
  } deriving Show

main :: IO ()
main = do
  print (Note { pitch = 0, duration = 1 })
