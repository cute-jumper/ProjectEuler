import           Control.Arrow
import           Data.Function
import           Data.List
import           Data.Ord

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

totient :: Int -> Double
totient n = product [ fromIntegral p / (fromIntegral p - 1) | p <- primeFactors n]

main :: IO ()
main = print . snd . maximumBy (comparing fst) . map (totient &&& id) $ [1..10^6]
