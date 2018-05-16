module Lib
    (
      taxicabDistance,
      makeInstructions,
      Instruction (..)
    ) where

import Data.List.Split

data Instruction = L Int | R Int deriving (Show, Eq, Ord)

taxicabDistance :: [Instruction] -> Int
taxicabDistance = impl origin
  where impl l [] = distance origin l
        impl l (i:is) = impl (locationAfter l i) is

makeInstructions :: String -> [Instruction]
makeInstructions line
  | line == "" = []
  | otherwise = map makeInstruction (splitOn ", " line)


-- Internals.

data Orientation = North | South | East | West deriving (Eq, Ord)
data Location = Location Int Int Orientation deriving (Eq, Ord)

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
