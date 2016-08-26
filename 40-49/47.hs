import           Data.List

primes :: [Int]
primes = 2:filter isPrime [3,5..]

primeFactors :: Int -> [Int]
primeFactors n = f n primes []
  where
    f 1 _ rs = rs
    f n (x:xs) rs
      | x > n = [n]
      | otherwise = if n `mod` x == 0
                    then f (g n x) xs (x:rs)
                    else f n xs rs
    g n x = if n `mod` x == 0
            then g (n `div` x) x
            else n


isPrime :: Int -> Bool
isPrime n = let mid = floor . sqrt . fromIntegral $ n
            in
              not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

main :: IO ()
main = print . head $ [ a | a <- [1..], let b = take 4 [a..],
                                            all ((==4) . length . primeFactors) b]
