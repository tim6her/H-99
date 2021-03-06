(%) :: Integral a => a -> a -> a
(%) a b = a - b * q
  where q = a `div` b

myGCD :: Integral a => a -> a -> a
myGCD 0 _ = 0
myGCD _ 0 = 0
myGCD 1 _ = 1
myGCD _ 1 = 1
myGCD x y
  | x < 0 = myGCD (-x) y
  | y < 0 = myGCD x (-y)
  | otherwise = if r == 0
                then y
                else myGCD y r
  where  r = x % y


main :: IO ()
main = print $ [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
