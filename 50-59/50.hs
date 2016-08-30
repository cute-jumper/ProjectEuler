import           Control.Arrow
import           Data.List
import           Data.Ord

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = let mid = floor . sqrt . fromIntegral $ n
            in
              not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

findPeak :: [Int] -> Int
findPeak xs = f 0 xs
  where
    f z [] = z
    f z (x:xs)
      | x >= z = f x xs
      | otherwise = z


maxConsecutive :: Int -> Int
maxConsecutive n = findPeak . map (f 0 n) . takeWhile ((<=n) . head) $ unfoldr (\s -> Just (s, tail s)) primes
  where
    f r n (x:xs)
      | n == 0 = r
      | n < x = 0
      | otherwise = f (r + 1) (n - x) xs

main :: IO ()
main = print . snd . maximumBy (comparing fst) . map (maxConsecutive &&& id) $
       takeWhile (<10^6) primes
