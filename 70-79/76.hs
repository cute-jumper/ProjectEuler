genNext :: [[Int]] -> [[Int]]
genNext a = ([ sum $ drop k (a !! k) | k <- [0..n-1]] ++ [1]) : a
  where
    n = length a

-- 1: [1]
-- 2: [1] ++ [1]
-- 3: [2,0] ++ [1]

main :: IO ()
main = print $ (sum . head . (!! 99) . iterate genNext) [[1]] - 1
