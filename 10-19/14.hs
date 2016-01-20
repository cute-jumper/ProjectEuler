import           Data.Function
import           Data.List

computeSeq::Int -> [Int]
computeSeq num
  | num == 1 = [1]
  | otherwise = num:computeSeq nextNum
    where
      nextNum = if odd num then  3 * num + 1 else num `div` 2

main :: IO ()
main = do
  print . head . maximumBy (compare `on` length) . map computeSeq
    $ [1..(truncate 1e6)-1]
