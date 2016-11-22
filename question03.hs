elementAt :: (Integral i) => [a] -> i -> a
elementAt [] _ = error "Index out of range"
elementAt (x:xs) i
    | i <= 0 = error "Non-positiv index"
    | i == 1 = x
    | otherwise = elementAt xs (i - 1)