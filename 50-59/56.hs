import           Data.Char

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

main :: IO ()
main = print . maximum $ [ digitSum (a^b) | a <- [1..100], b <- [1..100] ]
