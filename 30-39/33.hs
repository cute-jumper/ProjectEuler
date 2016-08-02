import           Data.List

isCurious :: (Int, Int) -> (Int, Int) -> Bool
isCurious (a, b) (c, d) = a * d == b * c

listOfCurious = [let d = gcd a c in (a `div` d, c `div` d) |
                  a <- [1..9], b <- [1..9], c <- [1..9],
                  isCurious (10 * a + b, 10 * b + c) (a, c), a /= c]

getDenominator :: Int
getDenominator = let (a, b) = foldl' f (1, 1) listOfCurious
                     d = gcd a b
                 in b `div` d
  where
    f (a, b) (c, d) = (a * c, b * d)
