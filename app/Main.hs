module Main where

import System.Environment

import Lib

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "day01a" -> do
      line <- getLine
      print $ taxicabDistance $ makeInstructions line
    "day01b" -> do
      line <- getLine
      print $ hqDistance $ makeInstructions line
    na -> putStrLn $ "Unknown command: " ++ na
