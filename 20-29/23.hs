-- Copied from 21.hs
import           Control.Monad
import           Data.List
import qualified Data.Set      as S

primeNumbers :: [Int]
primeNumbers = sieve $ 2:[3,5..]
  where
    sieve (hd:rest) = hd:(sieve $ filter (\x -> (x `mod` hd) /= 0) rest)

getPrimeFactors :: Int -> [Int]
getPrimeFactors x = getFactors x primeNumbers
  where
    getFactors x primes@(hd:rest)
      | x == 1 = []
      | x `mod` hd == 0 = hd:getFactors (x `div` hd) primes
      | otherwise = getFactors x rest

powerset :: [a] -> [[a]]
powerset = filterM (const [False, True])

properDivisorSum :: Int -> Int
properDivisorSum = sum . S.toList . S.fromList . init . map product . powerset . getPrimeFactors

n = 28123

getAbundants :: [Int]
getAbundants = filter (\x -> properDivisorSum x > x) [12..n]

abundantSet :: S.Set Int
abundantSet = S.fromList getAbundants

isSumOfAbundants :: Int -> Bool
isSumOfAbundants x = any (\y -> y < x `div` 2 + 1 && S.member (x - y) abundantSet) getAbundants

getNonAbundantsSum :: Int
getNonAbundantsSum = sum $ filter (not . isSumOfAbundants) [1..n]
