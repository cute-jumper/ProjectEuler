import           Data.Function
import           Data.List
import           Data.Ord
import           Data.Ratio

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = let mid = floor . sqrt . fromIntegral $ n
                in
                  not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

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

isPerm :: Int -> Int -> Bool
isPerm = ((==) `on` (sort . show))

primePairs :: Int -> [(Int, Int)]
primePairs n = [(a, b) | let sq = floor . sqrt . fromIntegral $ n,
                             let ps = reverse . dropWhile (<= sq `div` 2) . takeWhile (<= 2 * sq) $ primes,
                             a <- takeWhile (>= sq) ps,
                             b <- dropWhile (>= sq) ps,
                             a * b < n,
                             isPerm (a*b) ((a-1) * (b-1))]

main :: IO ()
main = print . (uncurry (*)) . minimumBy (comparing f) . primePairs $ 10^7
  where
    f (x, y) = ((/) `on` fromIntegral) (x * y)  ((x - 1) * (y - 1))
