insertAt :: a -> [a] -> Int -> [a]
insertAt _y [] _i = []
insertAt y xs 1 = y:xs
insertAt y all@(x:xs) i
  | i > length all = []
  | i < 1 = []
  | otherwise = x : (insertAt y xs (i - 1))

main :: IO ()
main = print $ insertAt 'X' "abcd" 2
