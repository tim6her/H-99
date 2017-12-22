import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs r = do
                    gen <- getStdGen
                    return $ rnd_select' gen xs r

rnd_select' :: (RandomGen g) => g -> [a] -> Int -> [a]
rnd_select' _g [] _r = []
rnd_select' g all@(x:xs) r
  | r > n = []
  | r <= 0 = []
  | otherwise = if d > r
                then rnd_select' gen xs r
                else x : (rnd_select' gen xs (r - 1))
    where
      n = length all
      (d, gen) = randomR (1, n) g

diff_select :: Int -> Int -> IO [Int]
diff_select r n = rnd_select [1..n] r

main :: IO ()
main = diff_select 6 49 >>= print
