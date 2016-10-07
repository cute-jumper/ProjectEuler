import           Control.Arrow
import           Data.Function
import           Data.List

diags :: [[Int]]
diags = [1]:unfoldr (\(x,n) -> Just ([x, x + n, x + 2 * n, x + 3 * n], (x + 4 * n + 2, n + 2))) (3,2)

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = let mid = floor . sqrt . fromIntegral $ n
                in
                  not . any ((==0) . (n `mod`)) . takeWhile (<=mid) $ primes

percents :: [Double]
percents = unfoldr f (0,0)
  where
    f (p, n) = Just (v, (p + p1, n + 1))
      where
        p1 = length . filter isPrime $ diags !! n
        v = ((/) `on` fromIntegral) (p + p1) (4 * n + 1)

main :: IO ()
main = print . fst . head . dropWhile ((>=0.1) . snd) . zip [3,5..] . tail $ percents
