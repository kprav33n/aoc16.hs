module Day01
  (
    makeInstructions,
    taxicabDistance,
    hqDistance,
    Instruction (..)
  ) where

import Data.List
import Data.List.Split
import qualified Data.Set as Set

data Instruction = L Int | R Int deriving (Show, Eq)

makeInstructions :: String -> [Instruction]
makeInstructions line
  | line == "" = []
  | otherwise = map makeInstruction (splitOn ", " line)

taxicabDistance :: [Instruction] -> Int
taxicabDistance = impl origin
  where impl l [] = distance origin l
        impl l (i:is) = impl (locationAfter l i) is

hqDistance :: [Instruction] -> Int
hqDistance = impl origin Set.empty
  where impl l _ [] = distance origin l
        impl l seen (i:is) =
            case find (`Set.member` seen) cs of
              Just (x, y) -> distance origin (Location x y North)
              Nothing -> impl (last ls) (Set.union seen $ Set.fromList cs) is
          where ls = allLocationsUntil l i
                cs = map (\(Location x y _) -> (x, y)) ls


-- Internals.

data Orientation = North | South | East | West
data Location = Location Int Int Orientation

origin :: Location
origin = Location 0 0 North

makeInstruction :: String -> Instruction
makeInstruction "" = undefined
makeInstruction (x : xs) =
  case x of
    'L' -> L n
    'R' -> R n
    _ -> undefined
  where n = read xs :: Int

distance :: Location -> Location -> Int
distance (Location x1 y1 _) (Location x2 y2 _) =
  abs (x1 - x2) + abs (y1 - y2)

northOf :: Int -> Int -> Int -> Location
northOf x y units = Location x (y + units) North

southOf :: Int -> Int -> Int -> Location
southOf x y units = Location x (y - units) South

eastOf :: Int -> Int -> Int -> Location
eastOf x y units = Location (x + units) y East

westOf :: Int -> Int -> Int -> Location
westOf x y units = Location (x - units) y West

locationAfter :: Location -> Instruction -> Location
locationAfter (Location x y o) i =
  case i of
      L a -> case o of
        North -> westOf x y a
        South -> eastOf x y a
        East -> northOf x y a
        West -> southOf x y a
      R a -> case o of
        North -> eastOf x y a
        South -> westOf x y a
        East -> southOf x y a
        West -> northOf x y a

allLocationsUntil :: Location -> Instruction -> [Location]
allLocationsUntil (Location x y o) i =
  case i of
      L q -> case o of
        North -> map (westOf x y) [1..q]
        South -> map (eastOf x y) [1..q]
        East -> map (northOf x y) [1..q]
        West -> map (southOf x y) [1..q]
      R q -> case o of
        North -> map (eastOf x y) [1..q]
        South -> map (westOf x y) [1..q]
        East -> map (southOf x y) [1..q]
        West -> map (northOf x y) [1..q]
