import           Control.Arrow
import           Data.List

primes :: [Integer]
primes = 2:filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = let mid = floor . sqrt . fromIntegral $ n
                in
                  not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

primeFactors :: Integer -> [Integer]
primeFactors n = f n primes []
  where
    f 1 _ rs = rs
    f n lst@(x:xs) rs
      | x > n = [n]
      | otherwise = if n `mod` x == 0
                    then f (n `div` x) lst (x:rs)
                    else f n xs rs

totient :: Integer -> Integer
totient n = product . map f $ ps
  where
    f (x, y) = x^(y-1) * (x-1)
    ps = map (head &&& length) . group $ primeFactors n

-- seems to be even slower... No idea why
-- totients :: Int -> [Int]
-- totients n = f 2 $ zip [2..n] [2..]
--   where
--     f k a
--       | k > n = map snd a
--       | fst kth == snd kth = f (k+1) [(x, z) |
--                                       (x, y) <- a,
--                                       let z = if x `mod` k == 0 then y * (k-1) `div` k else y]
--       | otherwise = f (k+1) a
--         where
--           kth = a !! (k - 2)

fareyCount :: Integer -> Integer
fareyCount n = sum . map totient $ [2..n]

main :: IO ()
main = print . fareyCount $ 10^6
