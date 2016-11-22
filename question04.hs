myLength :: (Integral i) => [a] -> i
myLength [] = 0
myLength (_:xs) = 1 + myLength xs