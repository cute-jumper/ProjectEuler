import           Data.List
import           Data.Ord

divide :: Int -> [Int]
divide d = recDivide 1 d
  where
    recDivide num divisor
      | num == 0 || num == divisor = []
      | num < divisor = recDivide (num * 10) divisor
      | num > divisor = let quot = num `div` divisor
                            rem = num - quot * divisor
                        in rem:recDivide (rem * 10) d

repeatLength :: Int -> Int
repeatLength x = f [] $ dropWhile (== 0) $ divide x
  where
    f _ [] = 0
    f acc (x:xs) = case elemIndex x acc of
                     Just v -> v + 1
                     Nothing -> f (x:acc) xs

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

main :: IO ()
main = print . (1+) $ maxIndex (map repeatLength [1..999])
