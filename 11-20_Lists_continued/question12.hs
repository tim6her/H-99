data Encoded a = Single a | Multiple Int a
  deriving Show

decodeModified :: (Eq a) => [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x:(decodeModified xs)
decodeModified ((Multiple n x):xs)
  | n == 0 = decodeModified xs
  | n == 1 = x:(decodeModified xs)
  | otherwise = x:(decodeModified ((Multiple (n-1) x):xs))

main :: IO ()
main = print decoded
  where decoded = decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
                                  Multiple 2 'a',Single 'd',Multiple 4 'e']
