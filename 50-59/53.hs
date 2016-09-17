countComb :: (Integral a) => a -> a -> a
countComb n r
  | 2 * r > n = countComb n (n - r)
  | otherwise = product [(n - r + 1)..n] `div` product [2..r]

main :: IO ()
main = print . length $ [ (n, r) | n <- [1..100],
                          r <- [0..n],
                          countComb n r > 1000000]
