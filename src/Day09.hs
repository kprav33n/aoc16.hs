module Day09 (decompress) where

import Control.Monad (replicateM)
import Text.ParserCombinators.Parsec

decompress :: String -> String
decompress = recur []
  where
    recur acc [] = acc
    recur acc s =
      case parse parseData "data" s of
        Left err -> undefined
        Right s ->
          case s of
            (Uncompressed d,r) -> recur (acc ++ d) r
            (Compressed d l i,r) -> recur (acc ++ take (l * i) (cycle d)) r

-- Parsers.

data Section
  = Uncompressed String
  | Compressed String
               Int
               Int
  deriving (Show)

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

-- FIXME: Is there an existing construct for parsing decimal integers?
int :: Parser Int
int = read <$> many digit
