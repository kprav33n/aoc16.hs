{-# LANGUAGE OverloadedStrings #-}

module Day06 (correctMessage) where

import Data.List (sortOn, transpose)
import Data.Map (fromListWith, toList)

correctMessage :: (String -> Char) -> String -> String
correctMessage f = map (topHit f) . transpose . lines

-- Internals

topHit :: (Ord a) => ([a] -> a) -> [a] -> a
topHit f = f . map fst . sortOn snd . frequency

frequency :: (Ord a) => [a] -> [(a,Int)]
frequency = toList . fromListWith (+) . map (\x -> (x,1))
