module Day05 (doorPassword) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (mapMaybe)

doorPassword :: Int -> String -> String
doorPassword n d = (take n . mapMaybe (passwordChar . (++) d . show)) ([0..]::[Int])

-- Internals.

md5Hash :: String -> String
md5Hash = show . md5 . C.pack

passwordChar :: String -> Maybe Char
passwordChar = recur (0::Int) . md5Hash
  where recur _ [] = undefined
        recur i (h:hs)
          | i == 5 = Just h
          | i < 5 && h == '0' = recur (i + 1) hs
          | otherwise = Nothing
