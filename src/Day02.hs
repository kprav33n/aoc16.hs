{-# LANGUAGE OverloadedStrings #-}

module Day02 (makeInstructions, bathroomCode) where

import qualified Data.Text as T

type Pos = (Int,Int)

data Instruction = U | D | R | L

makeInstructions :: String -> [[Instruction]]
makeInstructions ls =
  map (map makeInstruction . T.unpack) (T.splitOn "\n" $ T.strip text)
  where text = T.pack ls
        makeInstruction ch =
          case ch of
            'U' -> U
            'D' -> D
            'R' -> R
            'L' -> L
            _ -> undefined

bathroomCode :: [[Instruction]] -> String
bathroomCode is = fst $ foldl singleCode ("",(1,1)) is
  where singleCode (cs,p) i = (cs ++ c,(x,y))
          where (x,y) = executeInstruction p i
                c = show $ keypad!!x!!y

-- Internals.

keypad :: [[Int]]
keypad = [[1,2,3],[4,5,6],[7,8,9]]

width :: Int
width = length $ head keypad

height :: Int
height = length keypad

boundedPos :: Pos -> Maybe Pos
boundedPos (x,y)
  | x >= 0 && x < height && y >= 0 && y < width = Just (x,y)
  | otherwise = Nothing

nextPos :: Pos -> Instruction -> Maybe Pos
nextPos (x,y) i =
  boundedPos $ case i of
                 U -> (x-1,y)
                 D -> (x+1,y)
                 R -> (x,y+1)
                 L -> (x,y-1)

executeInstruction :: Pos -> [Instruction] -> Pos
executeInstruction p [] = p
executeInstruction p (i:is) =
  case nextPos p i of
    Just n -> executeInstruction n is
    Nothing -> executeInstruction p is
