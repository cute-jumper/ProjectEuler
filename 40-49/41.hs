import Data.List
import Data.Char

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = not . any ((==0) . (n `mod`)) .
                takeWhile (<(floor . sqrt . fromIntegral $ n)) $ primes

getPrimes :: Int -> [Int]
getPrimes n = [ x | x <- map read . permutations $ ['1'..intToDigit n],
                isPrime x]

main :: IO ()
main = print . maximum . head . dropWhile ((==0) . length) . map getPrimes $ [9,8..1]
