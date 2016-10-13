import           Data.Ratio

fareyCount :: Rational -> Rational -> Int
fareyCount a b = if d <= 12000
                 then
                   1 + fareyCount a mid + fareyCount mid b
                 else
                   0
  where
    n1 = numerator a
    n2 = numerator b
    d1 = denominator a
    d2 = denominator b
    mid = (n1 + n2) % (d1 + d2)
    n = numerator mid
    d = denominator mid

main :: IO ()
main = print $ fareyCount (1 % 3) (1 % 2)
