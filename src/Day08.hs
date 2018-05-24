module Day08
  ( parseInput
  , numLitPixelsAfter
  , screenAfter
  ) where

import Text.ParserCombinators.Parsec
import Data.List (transpose)

-- type Screen = [[Bool]]
newtype Screen = Screen [[Bool]]

instance Show Screen where
  show (Screen s) = unlines $ map (map (\x -> if x then '#' else '.')) s

data Instruction
  = Rect Int Int
  | RotateRow Int Int
  | RotateColumn Int Int
  deriving (Show)

parseInput :: String -> [Instruction]
parseInput ls = map f (lines ls)
  where
    f i =
      case parse parseInst "TBD" i of
        Left err -> undefined
        Right r -> r

screenAfter :: Int -> Int -> [Instruction] -> Screen
screenAfter c r = executeInstructions (newScreen c r)

numLitPixelsAfter :: Int -> Int -> [Instruction] -> Int
numLitPixelsAfter c r is = (length . filter (== True) . concat) s
  where Screen s = screenAfter c r is

-- Internals

newScreen :: Int -> Int -> Screen
newScreen c r = Screen (replicate r $ replicate c False)

executeInstruction :: Screen -> Instruction -> Screen
executeInstruction (Screen s) i =
  case i of
    Rect a b -> Screen (map ((++) (replicate a True) . drop a) (take b s) ++ drop b s)
    RotateRow a b -> Screen (take a s ++ [rotate b (s!!a)] ++ drop (a + 1) s)
    RotateColumn a b -> let t = transpose s in
      Screen (transpose $ take a t ++ [rotate b (t!!a)] ++ drop (a + 1) t)

executeInstructions :: Screen -> [Instruction] -> Screen
executeInstructions = foldl executeInstruction

-- Copied from https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop (length xs - n) (cycle xs)) xs

-- Parsers

parseInst :: Parser Instruction
parseInst = try parseRectInst <|> try parseRotRow <|> parseRotCol

parseRectInst :: Parser Instruction
parseRectInst = do
  string "rect"
  spaces
  a <- int
  char 'x'
  b <- int
  return $ Rect a b

parseRotRow :: Parser Instruction
parseRotRow = do
  string "rotate row y="
  (a,b) <- parseAxB
  return $ RotateRow a b

parseRotCol :: Parser Instruction
parseRotCol = do
  string "rotate column x="
  (a,b) <- parseAxB
  return $ RotateColumn a b

parseAxB :: Parser (Int, Int)
parseAxB = do
  a <- int
  spaces
  string "by"
  spaces
  b <- int
  return (a,b)

int :: Parser Int
int = read <$> many digit
