combinations :: Int -> [a] -> [[a]]
combinations r all@(x:xs)
  | r < 0 = []
  | r == 0 = [[]]
  | r > length all = []
  | r == length all = [all]
  | otherwise = fmap (x:) hasX ++ noX
    where
      hasX = combinations (r - 1) xs
      noX = combinations r xs


main :: IO ()
main = print $ combinations 3 "abcdef"
