module Main where

import System.Environment
import Data.Text (pack)

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

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
    "day03b" -> do
      contents <- getContents
      print $ numPossibleTriangles2 $ Day03.parseInput $ pack contents
    "day04a" -> do
      contents <- getContents
      print $ sectorSum (Day04.parseInput contents)
    "day04b" -> do
      contents <- getContents
      print $ northPoleSector (Day04.parseInput contents)
    "day05a" -> do
      line <- getLine
      putStrLn $ doorPassword 8 line
    "day05b" -> do
      line <- getLine
      putStrLn $ doorPassword2 8 line
    "day06a" -> do
      contents <- getContents
      putStrLn $ correctMessage contents
    na -> putStrLn $ "Unknown command: " ++ na
