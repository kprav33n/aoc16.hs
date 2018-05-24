module Day09
  (decompressedLength
  ,decompressedLength2) where

import Control.Monad (replicateM)
import Text.ParserCombinators.Parsec

decompressedLength :: String -> Int
decompressedLength = recur 0
  where
    recur acc [] = acc
    recur acc s =
      case parse parseData "data" s of
        Left err -> undefined
        Right s ->
          case s of
            (Uncompressed d,r) -> recur (acc + length d) r
            (Compressed d l i,r) -> recur (acc + i * l) r

decompressedLength2 :: String -> Int
decompressedLength2 = recur 0
  where
    recur acc [] = acc
    recur acc s =
      case parse parseData "data" s of
        Left err -> undefined
        Right s ->
          case s of
            (Uncompressed d,r) -> recur (acc + length d) r
            (Compressed d l i,r) -> recur (acc + i * recur 0 d) r

-- Parsers.

data Section
  = Uncompressed String
  | Compressed String
               Int
               Int

parseData :: Parser (Section,String)
parseData = try parseUncompressed <|> try parseCompressed

parseUncompressed :: Parser (Section,String)
parseUncompressed = do
  s <- many1 $ noneOf "("
  rest <- getInput
  return (Uncompressed s,rest)

parseCompressed :: Parser (Section,String)
parseCompressed = do
  char '('
  l <- int
  char 'x'
  i <- int
  char ')'
  s <- parseCData l
  rest <- getInput
  return (Compressed s l i,rest)

parseCData :: Int -> Parser String
parseCData i = replicateM i anyChar

int :: Parser Int
int = read <$> many digit
