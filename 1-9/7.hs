sieve (hd:rest) =
  hd:(sieve (filter (\x -> x `mod` hd /= 0) rest))

main = print $ sieve (2:[3,5..]) !! 10000
