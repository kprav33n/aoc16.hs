module Lib
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

locationAfter :: Location -> Instruction -> Location
locationAfter (Location x y o) i =
  case i of
      L a -> case o of
        North -> Location (x - a) y West
        South -> Location (x + a) y East
        East -> Location x (y + a) North
        West -> Location x (y - a) South
      R a -> case o of
        North -> Location (x + a) y East
        South -> Location (x - a) y West
        East -> Location x (y - a) South
        West -> Location x (y + a) North

allLocationsUntil :: Location -> Instruction -> [Location]
allLocationsUntil (Location x y o) i =
  case i of
      L q -> case o of
        North -> map (\a -> Location (x - a) y West) [1..q]
        South -> map (\a -> Location (x + a) y East) [1..q]
        East -> map (\a -> Location x (y + a) North) [1..q]
        West -> map (\a -> Location x (y - a) South) [1..q]
      R q -> case o of
        North -> map (\a -> Location (x + a) y East) [1..q]
        South -> map (\a -> Location (x - a) y West) [1..q]
        East -> map (\a -> Location x (y - a) South) [1..q]
        West -> map (\a -> Location x (y + a) North) [1..q]
