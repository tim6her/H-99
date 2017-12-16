repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = r ++ (repli xs n)
  where r = repliElem x n

repliElem :: a -> Int -> [a]
repliElem _ 0 = []
repliElem x n = x : (repliElem x (n - 1))

main :: IO ()
main = print $ repli "abc" 3
