encode :: (Eq a) =>  [a] -> [(Integer, a)]
encode [] = error "Nothing to encode"
encode [x] = [(1, x)]
encode all@(x:_)
    | length r > 0 = (xx, x) : encode r
    | otherwise = (xx, x) : []
    where
        (xx, r) = encode' x all
    
encode' :: (Eq a) => a -> [a] -> (Integer, [a])
encode' _ [] = (0, [])
encode' x all@(y:ys)
    | x == y = (1 + xx, r)
    | otherwise = (0, all)
    where
        (xx, r) = encode' x ys