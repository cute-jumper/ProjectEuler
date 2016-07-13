fib :: [Integer]
fib = fib' 1 1
  where
    fib' x y = x:fib' y (x + y)

-- CAUTION: Can't use Double!
main :: IO ()
main = print . (1+) . length . takeWhile (< 10^999) $ fib
