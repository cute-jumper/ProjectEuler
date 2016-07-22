import           Data.Char
import           Data.List

fadd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
fadd (n, _) (x, y) = (n * x + y, x)

valueAt :: Int -> [Integer] -> (Integer, Integer)
valueAt n = foldr1 fadd . map (\x -> (x, 1)) . take n

genSeq :: [Integer]
genSeq = map f [0..]
  where
    f 0 = 2
    f x
      | x `mod` 3 == 2 = (x `div` 3 + 1) * 2
      | otherwise = 1

main :: IO ()
main = print . sum . map digitToInt . show . fst . valueAt 100 $ genSeq
