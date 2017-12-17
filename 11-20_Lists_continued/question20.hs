removeAt :: Int -> [a] -> [a]
removeAt i all@(x:xs)
  | i <= 0 = []
  | i > length all = []
  | i == 1 = xs
  | otherwise = x : (removeAt (i - 1) xs)

main :: IO ()
main = print $ removeAt 2 "abcd"
