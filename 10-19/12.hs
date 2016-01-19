import           Data.List

sieve::[Int] -> [Int]
sieve (hd:rest) =
  hd:(sieve (filter (\x -> x `mod` hd /= 0) rest))

iter::Int->[Int]->[Int]
iter n (hd:rest)
  | n < hd = []
  | n == hd = [n]
  | n `mod` hd == 0 = hd:iter (n `div` hd) (hd:rest)
  | otherwise = iter n rest

getPrimeFactors::Int->[Int]
getPrimeFactors num =
  iter num . sieve $ 2:[3,5..]

getNumberOfFactors::Int->Int
getNumberOfFactors = product . map ((+1) . length) . group . getPrimeFactors

main :: IO ()
main = do
  print . head . dropWhile (\x -> getNumberOfFactors x <= 500) $ triangular
  where
    triangular = scanl (+) 1 [2..]
