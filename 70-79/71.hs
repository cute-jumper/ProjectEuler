import           Data.Ratio

next :: Rational -> Rational -> Rational
next a b = (n1 + n2) % (m1 + m2)
  where
    n1 = numerator a
    m1 = denominator a
    n2 = numerator b
    m2 = denominator b

main :: IO ()
main = print . numerator . last . takeWhile ((<=10^6) . denominator) $ iterate (next (3 % 7)) (2 % 5)
