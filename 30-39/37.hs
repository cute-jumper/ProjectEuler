import           Data.List

primes :: [Int]
primes = (2:filter isPrime [3,5..])

isPrime :: Int -> Bool
isPrime x
  | x < 2 = False
  | otherwise = not . any (\y -> x `mod` y == 0) $ takeWhile (\y -> y <= (floor . sqrt . fromIntegral $ x)) primes

trunctables :: Int -> [Int]
trunctables x = map read $ map (`take` xs) [1..length xs] ++ map (`drop` xs) [1..length xs - 1]
  where
    xs = show x

isTrunctablePrime :: Int -> Bool
isTrunctablePrime = all isPrime . trunctables

trunctablePrimes :: [Int]
trunctablePrimes = filter isTrunctablePrime [9..]

main :: IO ()
main = print . sum $ take 11 trunctablePrimes
