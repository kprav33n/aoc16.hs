{-# LANGUAGE OverloadedStrings #-}

module Day06 (correctMessage) where

import Data.List (sortOn, transpose)
import Data.Map (fromListWith, toList)

correctMessage :: String -> String
correctMessage = map topHit . transpose . lines

-- Internals

topHit :: (Ord a ) => [a] -> a
topHit = fst . last . sortOn snd . frequency

frequency :: (Ord a) => [a] -> [(a,Int)]
frequency = toList . fromListWith (+) . map (\x -> (x,1))
