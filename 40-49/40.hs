import Data.Char

getNumAt :: Int -> Int
getNumAt n = f n 1 0
  where
    f x y z
      | x == 0 = z `mod` 10
      | x < y = digitToInt $ reverse (show (z + 1)) !! (y - x)
      | x <= digits = f (x `mod` y) y (z + (x `div` y))
      | otherwise = f (x - digits) (y + 1) (z + nums)
      where
        nums = (10^y - 10^(y - 1))
        digits = y * nums

main :: IO ()
main = print . product . map (getNumAt . (10^)) $ [0..6]
