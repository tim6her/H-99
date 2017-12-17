slice :: [a] -> Int -> Int -> [a]
slice xs fst lst = cropHead (cropTail xs lst) fst

cropTail :: [a] -> Int -> [a]
cropTail _ 0 = []
cropTail [] _ = []
cropTail (x:xs) lst
  | lst < 0 = []
  | otherwise = x : (cropTail xs (lst - 1))

cropHead :: [a] -> Int -> [a]
cropHead [] _ = []
cropHead all@(x:xs) fst
  | fst > length all = []
  | fst <= 1 = all
  | otherwise = cropHead xs (fst - 1)

main :: IO ()
main = print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
