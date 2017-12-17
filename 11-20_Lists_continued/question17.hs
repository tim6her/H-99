split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n
  | n < 0 = ([], [])
  | otherwise = (x:p1, p2)
    where
      (p1, p2) = split xs (n-1)

main :: IO ()
main = print $ split "abcdefghik" 3
