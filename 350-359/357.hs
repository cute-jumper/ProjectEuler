import           Data.List

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = let mid = floor . sqrt . fromIntegral $ n
                in
                  not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

isGen :: Int -> Bool
isGen n = all isPrime [ k + n `div` k | k <- [1..h], n `mod` k == 0]
  where
    h = floor . sqrt . fromIntegral $ n

main :: IO ()
main = print . sum . filter isGen $ [1..10^8]
