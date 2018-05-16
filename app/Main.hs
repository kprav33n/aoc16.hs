module Main where

import Lib

main :: IO ()
main = do
  line <- getLine
  print $ taxicabDistance $ makeInstructions line
