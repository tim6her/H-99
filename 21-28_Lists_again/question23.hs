import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs r = do
                    gen <- getStdGen
                    return $ rnd_select' gen xs r

rnd_select' :: (RandomGen g) => g -> [a] -> Int -> [a]
rnd_select' _g [] _r = []
rnd_select' g xs r = xx
  where
    n = length xs - 1
    ii = take r (randomRs (0, n) g)
    xx = [xs !! i | i <- ii]


main :: IO ()
main = do rnd_select "abcdefgh" 3 >>= print
