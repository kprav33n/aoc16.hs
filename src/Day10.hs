module Day10
  (parseInput
  ,statesAfter
  ,findBotFor
  ,chipsProduct) where

import Text.ParserCombinators.Parsec
import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified Control.Monad.State as SM

data Entity
  = Bot Int
  | Output Int
  deriving (Show,Eq,Ord)

data Instruction
  = Assign Int Entity
  | Give Entity Entity Entity
  deriving (Show)

type OutputState = (Int,[Int])

type EntityStateMap = Map.Map Entity [Int]
type EntityState = (Entity,[Int])

parseInput :: String -> [Instruction]
parseInput = map mustParseInstruciton . lines

applyAssignment :: Instruction -> SM.State (EntityStateMap,EntityState) EntityState
applyAssignment i = do
  (esMap, eState) <- SM.get
  case i of
    Assign n entity ->
      SM.put (newESMap, newEState) >> return newEState
      where
        newState = giveToEntity n (Map.findWithDefault [] entity esMap)
        newESMap = Map.insert entity newState esMap
        newEState = (entity, newState)
    _ -> return eState

applyAssignments :: [Instruction] -> SM.State (EntityStateMap,EntityState) [EntityState]
applyAssignments = mapM applyAssignment

applyComparison :: Instruction -> SM.State (EntityStateMap,EntityState) EntityState
applyComparison i = do
  (esMap, eState) <- SM.get
  case i of
    Give f l h ->
      let fState = Map.findWithDefault [] f esMap in
        case fState of
          [x,y] ->
            SM.put (newESMap, newEState) >> return newEState
            where
              newLState = giveToEntity (min x y) (Map.findWithDefault [] l esMap)
              newHState = giveToEntity (max x y) (Map.findWithDefault [] h esMap)
              newESMap = Map.insert f [] (Map.insert h newHState (Map.insert l newLState esMap))
              newEState = (f, fState)
          _ -> return eState
    _ -> return eState

applyComparisons :: [Instruction] -> SM.State (EntityStateMap,EntityState) [EntityState]
applyComparisons = mapM applyComparison

statesAfter :: [Instruction] -> [EntityState]
statesAfter is =
  eState ++ SM.evalState (applyComparisons (cycle is)) st
  where
    (eState, st) = SM.runState (applyAssignments is) (Map.empty, (Bot 0, []))

findBotFor :: Int -> Int -> [EntityState] -> Int
findBotFor _ _ [] = undefined
findBotFor a b (x:xs) =
  if length (snd x) == 2 && min a b == minimum (snd x) && max a b == maximum (snd x)
  then case fst x of
    Bot b -> b
    _ -> undefined
  else findBotFor a b xs

chipsProduct :: [Instruction] -> Int
chipsProduct is =
  recur (cycle is) st
  where
    (eState, st) = SM.runState (applyAssignments is) (Map.empty, (Bot 0, []))
    recur (i:is) st =
      case (Map.lookup (Output 0) esMap
           ,Map.lookup (Output 1) esMap
           ,Map.lookup (Output 2) esMap) of
        (Just [x],Just [y],Just [z]) -> x * y * z
        _ -> recur is (SM.execState (applyComparison i) st)
      where esMap = fst st

-- Internals.

mustParseInstruciton :: String -> Instruction
mustParseInstruciton s =
  case parse parseInstruction "instruction" s of
    Left err -> undefined
    Right i -> i

giveToEntity :: Int -> [Int] -> [Int]
giveToEntity i es = i:es

-- Parsers.

parseInstruction :: Parser Instruction
parseInstruction = try parseAssign <|> try parseGive

parseAssign :: Parser Instruction
parseAssign = do
  string "value "
  v <- int
  string " goes to "
  e <- parseEntity
  return $ Assign v e

parseGive :: Parser Instruction
parseGive = do
  x <- parseBot
  string " gives low to "
  y <- parseEntity
  string " and high to "
  z <- parseEntity
  return $ Give x y z

parseEntity :: Parser Entity
parseEntity = try parseBot <|> try parseOut

parseBot :: Parser Entity
parseBot = do
  string "bot "
  v <- int
  return $ Bot v

parseOut :: Parser Entity
parseOut = do
  string "output "
  v <- int
  return $ Output v

int :: Parser Int
int = read <$> many digit
