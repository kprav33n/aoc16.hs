module Day03 (parseInput, numPossibleTriangles, numPossibleTriangles2) where

import Data.Text (Text, lines, words, strip)
import Data.Text.Read (decimal)

unsafeDecimal :: Text -> Int
unsafeDecimal = fst . either error id . decimal

parseInput :: Text -> [[Int]]
parseInput s =
  map (map (unsafeDecimal . strip) . Data.Text.words) (Data.Text.lines s)

numPossibleTriangles :: [[Int]] -> Int
numPossibleTriangles = length . filter isTriangle

numPossibleTriangles2 :: [[Int]] -> Int
numPossibleTriangles2 = length . filter isTriangle . concatMap rotateTrips . tripIt

-- Internals.

isTriangle :: [Int] -> Bool
isTriangle [a,b,c] = a + b > c && a + c > b && b + c > a
isTriangle _ = undefined

tripIt :: Show a => [[a]] -> [[[a]]]
tripIt [a,b,c] = [[a,b,c]]
tripIt xs = take 3 xs : tripIt (drop 3 xs)

rotateTrips :: Show a => [[a]] -> [[a]]
rotateTrips [a,b,c] =
  [[head a, head b, head c],
    [a!!1,b!!1,c!!1],
    [a!!2,b!!2,c!!2]]
rotateTrips _ = undefined
