data Encoded a = Single a | Multiple Int a
  deriving Show

pack :: (Eq a) =>  [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack all@(x:_) = xx : pack r
    where
        (xx, r) = pack' x all

pack' :: (Eq a) => a -> [a] -> ([a], [a])
pack' _ [] = ([], [])
pack' x all@(y:ys)
    | x == y = ((x:xx), r)
    | otherwise = ([], all)
    where
        (xx, r) = pack' x ys

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified p = foldr encodeElement [] (pack p)

encodeElement :: [a] -> [Encoded a] -> [Encoded a]
encodeElement [x] enc= (Single x):enc
encodeElement all@(x:xs) enc = (Multiple (length all) x):enc

main :: IO ()
main = print $ encodeModified "aaaabccaadeeee"
