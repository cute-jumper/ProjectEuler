isPrime::Int -> Bool
isPrime n
  | n == 2 = True
  | otherwise =
    let upper = floor . sqrt . fromIntegral $ n
    in not . any (\x -> n `mod` x == 0) $ 2:[3, 5..upper]

sumPrimes n =
  sum . filter (\x -> isPrime x) $ 2:[3, 5.. n]
