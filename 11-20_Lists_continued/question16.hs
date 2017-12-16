dropEvery :: [a] -> Int -> [a]
dropEvery xs n = everyNth xs n n

everyNth :: [a] -> Int -> Int -> [a]
everyNth [] _n _i = []
everyNth (x:xs) n 1 = everyNth xs n n
everyNth (x:xs) n i
  | n <= 1 = []
  | i <= 1 = []
  | otherwise = x : (everyNth xs n (i - 1))

main :: IO ()
main = print $ dropEvery "abcdefghik" 3
