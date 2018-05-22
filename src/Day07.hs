module Day07
  ( countIPs,
    scanABBA,
  ) where

countIPs :: String -> Int
countIPs = length . filter (\x -> scanABBA x == (True,False)) . lines

-- Internals

scanABBA :: String -> (Bool,Bool)
scanABBA x = recur False (False,False) x
  where
    recur h (o,i) (a:b:c:d:rest)
      | a == d && b == c && a /= b =
          case (h,o,i) of
            (False,False,True) -> (True,True)
            (False,False,False) -> recur h (True,False) r
            (True,False,False) -> recur h (False,True) r
            (True,True,False) -> (True,True)
            _ -> recur h (o,i) r
      | a == '[' || a == ']' = recur (not h) (o,i) r
      | otherwise = recur h (o,i) (b:c:d:rest)
      where
        r = (b:c:d:rest)
    recur _ t _ = t
