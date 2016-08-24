primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = let mid = floor . sqrt . fromIntegral $ n
            in
              not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

oddComposites :: [Int]
oddComposites = filter (not . isPrime) [3,5..]

isSquare :: Int -> Bool
isSquare n = r * r == n
  where
    r = floor . sqrt . fromIntegral $ n

isValidComb :: Int -> Int -> Bool
isValidComb c p = isSquare ((c - p) `div` 2)

isValid :: Int -> Bool
isValid n = any (isValidComb n) . takeWhile (<n) $ primes

main :: IO ()
main = print . head . dropWhile isValid $ oddComposites
