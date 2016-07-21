import           Control.Arrow
import           Data.List

limit = 100

listOfPows :: Int -> ([Int], Int)
listOfPows n = recConcat [] ([], 1)
  where
    recConcat xs (added, depth)
      | n^depth > limit = (added, length xs)
      | otherwise = recConcat ((map (*depth) [limit, limit - 1..2]) `union` xs)
                    (n^depth:added, depth + 1)

getNumOfDistinctPows :: Int
getNumOfDistinctPows = snd $ foldl' f ([], 0) [2..limit]
  where
    f (added, acc) x
      | x `elem` added = (added, acc)
      | otherwise = let (g1, g2) = ((++) *** (+)) $ listOfPows x
                    in (g1 added, g2 acc)
