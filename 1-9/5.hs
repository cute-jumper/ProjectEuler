smallestMultiple a b =
  let
    gcd a 0 = a
    gcd a b = gcd b (a `mod` b)
  in a `div` (gcd a b) * b

main = print $ foldl1 smallestMultiple [1..20]
