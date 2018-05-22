module Day05 (doorPassword, doorPassword2) where

import Crypto.Hash (MD5, Digest, hash)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)


doorPassword :: Int -> String -> String
doorPassword n d = (take n . map fst . mapMaybe (passwordChar2 . (++) d . show)) nonNeg

doorPassword2 :: Int -> String -> String
doorPassword2 n d = recur (emptyPassword n) (mapMaybe (passwordChar2 . (++) d . show) nonNeg)
  where recur p (t:ts)
          | isComplete p = p
          | otherwise = recur (uncurry updatePos t p) ts
        recur _ _ = undefined

-- Internals.

nonNeg :: [Int]
nonNeg = [0..]

md5Strict :: C.ByteString -> Digest MD5
md5Strict = hash

md5Hash :: String -> String
md5Hash = show . md5Strict . C.pack

passwordChar2 :: String -> Maybe (Char,Char)
passwordChar2 = recur (0::Int) . md5Hash
  where recur i (p:c:hs)
          | i == 5 = Just (p, c)
          | i < 5 && p == '0' = recur (i + 1) (c:hs)
          | otherwise = Nothing
        recur _ _ = undefined

type Password = String

emptyPassword :: Int -> Password
emptyPassword n = replicate n '-'

isComplete :: Password -> Bool
isComplete p = '-' `notElem` p

updatePos :: Char -> Char -> Password -> Password
updatePos n c p
  | i < l = take i p ++ [new] ++ drop (i + 1) p
  | otherwise = p
  where l = length p
        i = digitToInt n
        old = p!!i
        new = if old == '-' then c else old
