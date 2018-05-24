module Day07
  ( countTLSSupportedIPs,
    countSSLSupportedIPs,
  ) where

countTLSSupportedIPs :: String -> Int
countTLSSupportedIPs = length . filter (\x -> scanABBA x == (True,False)) . lines

countSSLSupportedIPs :: String -> Int
countSSLSupportedIPs = length . filter supportsSSL . lines

-- Internals

scanABBA :: String -> (Bool,Bool)
scanABBA = recur False (False,False)
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
        r = b:c:d:rest
    recur _ t _ = t

supportsSSL :: String -> Bool
supportsSSL l =
  let (snets,hnets) = abas l in
      any (\[a,b,_] -> [b,a,b] `elem` hnets) snets

abas :: String -> ([String],[String])
abas = recur False [] []
  where
    recur h snets hnets (a:b:c:rest)
      | a == '[' || a == ']' = recur (not h) snets hnets remaining
      | not h && a == c = recur h ([a,b,c]:snets) hnets remaining
      | h && a == c = recur h snets ([a,b,c]:hnets) remaining
      | otherwise = recur h snets hnets (b:c:rest)
      where
        remaining = b:c:rest
    recur _ snets hnets _ = (snets,hnets)
