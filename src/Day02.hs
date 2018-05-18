{-# LANGUAGE OverloadedStrings #-}

module Day02 (makeInstructions, bathroomCode, bathroomDiamondCode) where

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
bathroomCode is = fst $ foldl singleCode ("",(2,2)) is
  where singleCode (cs,p) i = (cs ++ [c],(x,y))
          where (x,y) = executeInstruction simpleKeypad p i
                c = simpleKeypad!!x!!y

bathroomDiamondCode :: [[Instruction]] -> String
bathroomDiamondCode is = fst $ foldl singleCode ("",(3,1)) is
  where singleCode (cs,p) i = (cs ++ [c],(x,y))
          where (x,y) = executeInstruction complexKeypad p i
                c = complexKeypad!!x!!y

-- Internals.

simpleKeypad :: [[Char]]
simpleKeypad = [['0','0','0','0','0']
               ,['0','1','2','3','0']
               ,['0','4','5','6','0']
               ,['0','7','8','9','0']
               ,['0','0','0','0','0']]

complexKeypad :: [[Char]]
complexKeypad = [['0','0','0','0','0','0','0']
                ,['0','0','0','1','0','0','0']
                ,['0','0','2','3','4','0','0']
                ,['0','5','6','7','8','9','0']
                ,['0','0','A','B','C','0','0']
                ,['0','0','0','D','0','0','0']
                ,['0','0','0','0','0','0','0']]

boundedPos :: [[Char]] -> Pos -> Maybe Pos
boundedPos kp (x,y)
  | kp!!x!!y /= '0' = Just (x,y)
  | otherwise = Nothing

nextPos :: [[Char]] -> Pos -> Instruction -> Maybe Pos
nextPos kp (x,y) i =
  boundedPos kp $ case i of
                 U -> (x-1,y)
                 D -> (x+1,y)
                 R -> (x,y+1)
                 L -> (x,y-1)

executeInstruction :: [[Char]] -> Pos -> [Instruction] -> Pos
executeInstruction kp p [] = p
executeInstruction kp p (i:is) =
  case nextPos kp p i of
    Just n -> executeInstruction kp n is
    Nothing -> executeInstruction kp p is
