module Day04 (parseInput, sectorSum) where

import Data.List (sort, sortBy)
import Data.Map (fromListWith, toList)

type Entry = (String,Int,String)

parseInput :: String -> [Entry]
parseInput = map readEntry . lines

sectorSum :: [Entry] -> Int
sectorSum = sum . map sector . filter isRealRoom


-- Internals.

readEntry :: String -> Entry
readEntry l = readTokens $ delimit [] [] [] (reverse l)

delimit :: String -> String -> String -> String -> [String]
delimit ck sec name [] = [name,sec,ck]
delimit [] _ _ (x:xs) =
  case x of
    ']' -> delimit [] [] [] xs
    _ -> delimit [x] [] [] xs
delimit ck [] _ (x1:x2:xs) =
  case x1 of
    '[' -> delimit ck [x2] [] xs
    _ -> delimit (x1:ck) [] [] (x2:xs)
delimit ck sec [] (x1:x2:xs) =
  case x1 of
    '-' -> delimit ck sec [x2] xs
    _ -> delimit ck (x1:sec) [] (x2:xs)
delimit ck sec name (x:xs) = delimit ck sec (x:name) xs

readTokens :: [String] -> Entry
readTokens [name,sec,ck] = (name,read sec,ck)
readTokens _ = undefined

top5 :: (Ord a, Show a) => [a] -> [a]
top5 = sort . take 5 . map fst . sortBy sortOcc . frequency

frequency :: (Ord a) => [a] -> [(a,Int)]
frequency = toList . fromListWith (+) . map (\x -> (x,1))

sortOcc :: Ord a => (a,Int) -> (a,Int) -> Ordering
sortOcc (a1,i1) (a2,i2)
  | i1 > i2 = LT
  | i1 < i2 = GT
  | otherwise = compare a1 a2

isRealRoom :: Entry -> Bool
isRealRoom (name,_,ck) = (top5 . stripDash) name == sort ck
  where stripDash = filter (/= '-')

sector :: Entry -> Int
sector (_,sec,_) = sec
