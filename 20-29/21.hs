import           Control.Monad
import           Data.List
import qualified Data.Set      as S

primeNumbers::[Integer]
primeNumbers = sieve $ 2:[3,5..]
  where
    sieve (hd:rest) = hd:(sieve $ filter (\x -> (x `mod` hd) /= 0) rest)

powerset :: [a] -> [[a]]
powerset = filterM (const [False, True])

getPrimeFactors::Integer -> [Integer]
getPrimeFactors x = getFactors x primeNumbers
  where
    getFactors x primes@(hd:rest)
      | x == 1 = []
      | x `mod` hd == 0 = hd:getFactors (x `div` hd) primes
      | otherwise = getFactors x rest

properDivisorSum::Integer -> Integer
properDivisorSum = sum . S.toList . S.fromList . init . map product . powerset . getPrimeFactors

getAmicableSums::Integer
getAmicableSums = foldl' f 0 [2..10000]
  where
    f acc x = let s = properDivisorSum x
              in if s > x && s <= 10000 && properDivisorSum s == x
                 then acc + x + s
                 else acc
