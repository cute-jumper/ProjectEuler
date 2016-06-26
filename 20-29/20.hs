import           Data.Char

sumFactorialDigit::Int
sumFactorialDigit = sum . map digitToInt . show . product $ [1..100]
