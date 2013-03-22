sieve (hd:rest) =
  hd:(sieve (filter (\x -> x `mod` hd /= 0) rest))

maxPrimeFactorOf n =
  let
    iter n primes@(hd:rest)
      | n == hd = n
      | n `mod` hd == 0 = iter (n `div` hd) primes
      | otherwise = iter n rest
  in iter n (sieve $ 2:[3,5..])
