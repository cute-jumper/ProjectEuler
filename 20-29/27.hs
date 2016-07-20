import           Data.List
import           Data.Ord

primeNumbers::[Integer]
primeNumbers = sieve $ 2:[3,5..]
  where
    sieve (hd:rest) = hd:(sieve $ filter (\x -> (x `mod` hd) /= 0) rest)

isPrime :: Integer -> Bool
isPrime x
  | x <= 0 = False
  | otherwise = f x (2:[3,5..(x `div` 2)])
  where
    f 2 _ = True
    f x [] = True
    f x (e:es)
      | x `mod` e == 0 = False
      | otherwise = f x es

computeNumOfPrimes a b = length . takeWhile isPrime $ [n * n + a * n + b | n <- [0..]]

getMaxNumEffProduct = snd $ maximumBy (comparing fst) [(computeNumOfPrimes a b, a * b) |
                       b <- takeWhile (<1000) $ dropWhile (<= 41) primeNumbers,
                       a <- [-b, -b + 2 .. b]]
