data Encoded a = Single a | Multiple Int a
  deriving Show

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = singles . encodeModified'

encodeModified' :: (Eq a) => [a] -> [Encoded a]
encodeModified' [] = []
encodeModified' [x] = [Multiple 1 x]
encodeModified' (x:xs)
  | x == y = (Multiple (n + 1) x): ys
  | otherwise = (Multiple 1 x):all
  where all@((Multiple n y):ys) = encodeModified' xs

singles :: [Encoded a] -> [Encoded a]
singles [] = []
singles ((Multiple 1 x):xs) = (Single x):(singles xs)
singles (mx@(Multiple n x):xs) = mx:(singles xs)
