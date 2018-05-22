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

simpleKeypad :: [String]
simpleKeypad = ["00000"
               ,"01230"
               ,"04560"
               ,"07890"
               ,"00000"]

complexKeypad :: [String]
complexKeypad = ["0000000"
                ,"0001000"
                ,"0023400"
                ,"0567890"
                ,"00ABC00"
                ,"000D000"
                ,"0000000"]

boundedPos :: [String] -> Pos -> Maybe Pos
boundedPos kp (x,y)
  | kp!!x!!y /= '0' = Just (x,y)
  | otherwise = Nothing

nextPos :: [String] -> Pos -> Instruction -> Maybe Pos
nextPos kp (x,y) i =
  boundedPos kp $ case i of
                 U -> (x-1,y)
                 D -> (x+1,y)
                 R -> (x,y+1)
                 L -> (x,y-1)

executeInstruction :: [String] -> Pos -> [Instruction] -> Pos
executeInstruction kp p [] = p
executeInstruction kp p (i:is) =
  case nextPos kp p i of
    Just n -> executeInstruction kp n is
    Nothing -> executeInstruction kp p is
