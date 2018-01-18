import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs r = do
                    gen <- getStdGen
                    return $ rnd_select' gen xs r

rnd_select' :: (RandomGen g) => g -> [a] -> Int -> [a]
rnd_select' _g [] _r = []
rnd_select' g all@(x:xs) r
  | r > length all = []
  | r == length all = all
  | r <= 0 = []
-- the probability of x being selected is 1 - (n - 1)^r / n^r
  | otherwise = if d > (n - 1) ^ r
                then x : rnd_select' g' all (r - 1)
                else rnd_select' g' xs r
  where
    n = length all
    (d, g') = randomR (1, n ^ r) g


main :: IO ()
main = do rnd_select "abcdefgh" 3 >>= print
