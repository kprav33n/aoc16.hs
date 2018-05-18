module Main where

import System.Environment

import Day01
import Day02

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
    na -> putStrLn $ "Unknown command: " ++ na
