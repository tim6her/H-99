range :: Int -> Int -> [Int]
range fst lst
  | fst > lst = []
  | fst == lst = [fst]
  | otherwise = fst : (range (fst + 1) lst)

main :: IO ()
main = print $ range 4 9
