module Main where

import System.Environment
import Data.Text (pack)

import Day01
import Day02
import Day03

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "day01a" -> do
      line <- getLine
      print $ taxicabDistance $ Day01.makeInstructions line
    "day01b" -> do
      line <- getLine
      print $ hqDistance $ Day01.makeInstructions line
    "day02a" -> do
      contents <- getContents
      putStrLn $ bathroomCode $ Day02.makeInstructions contents
    "day02b" -> do
      contents <- getContents
      putStrLn $ bathroomDiamondCode $ Day02.makeInstructions contents
    "day03a" -> do
      contents <- getContents
      print $ numPossibleTriangles $ Day03.parseInput $ pack contents
    na -> putStrLn $ "Unknown command: " ++ na
