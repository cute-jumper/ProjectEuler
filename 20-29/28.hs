numOfN :: Int -> Int
numOfN n
  | n == 1 = 1
  | otherwise = 4 * (2 * n - 3)^2 + 20 * (n - 1)

getSum :: Int -> Int
getSum h = sum $ map numOfN [1..(h + 1) `div` 2]

main :: IO ()
main = print $ getSum 1001
