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