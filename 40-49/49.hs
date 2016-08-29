import           Data.List

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = let mid = floor . sqrt . fromIntegral $ n
            in
              not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

validPrimes :: [Int]
validPrimes = takeWhile (<10000) . dropWhile (<1000) $ primes

tryPrime :: Int -> [String]
tryPrime n = [ s1 ++ s2 ++ s3 | a <- takeWhile (<n) validPrimes,
               b <-  dropWhile (<=n) validPrimes,
               a + b == 2 * n,
               let s1 = show a
                   s3 = show b,
                   sort s1 == s,
                   sort s3 == s]
  where
    s2 = show n
    s = sort s2

main :: IO ()
main = print . take 2 . filter (/= []) . map tryPrime $ validPrimes
