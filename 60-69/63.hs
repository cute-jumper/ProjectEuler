import           Control.Arrow
import           Data.List

numsOfN :: Integer -> [Integer]
numsOfN n = takeWhile (<10^n) . dropWhile (<10^(n-1)) . map (^n) $ [1..]

main :: IO ()
main = print . sum . unfoldr f $ 1
  where
    f n = case numsOfN n of
            xs@(x:_) -> Just (length xs, n+1)
            _ -> Nothing
