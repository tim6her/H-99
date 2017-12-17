rotate :: [a] -> Int -> [a]
rotate xs i = p2 ++ p1
  where (p1, p2) = split xs i

split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split all@(x:xs) n
  | n < 0 = split all (n + length all)
  | otherwise = (x:p1, p2)
    where
      (p1, p2) = split xs (n-1)

main :: IO ()
main = do
         print (rotate ['a','b','c','d','e','f','g','h'] 3)
         print (rotate ['a','b','c','d','e','f','g','h'] (-2))
