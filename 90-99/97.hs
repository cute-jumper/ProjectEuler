main :: IO ()
main = print $ (28433 * (2^7830457 `mod` 10^10) + 1) `mod` 10^10
