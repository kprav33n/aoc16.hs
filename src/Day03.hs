module Day03 (parseInput, numPossibleTriangles) where

import Data.Text (Text, lines, words, strip)
import Data.Text.Read (decimal)

unsafeDecimal :: Text -> Int
unsafeDecimal = fst . either error id . decimal

parseInput :: Text -> [[Int]]
parseInput s =
  map (map (unsafeDecimal . strip) . Data.Text.words) (Data.Text.lines s)

numPossibleTriangles :: [[Int]] -> Int
numPossibleTriangles = length . filter isTriangle

-- Internals.

isTriangle :: [Int] -> Bool
isTriangle [a,b,c] = a + b > c && a + c > b && b + c > a
isTriangle _ = undefined
