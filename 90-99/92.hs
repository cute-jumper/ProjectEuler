import           Data.Char

squareDigits :: Int -> Int
squareDigits n = sum . map ((^2) . digitToInt) $ show n

chainStop :: Int -> Int
chainStop n
  | n == 1 || n == 89 = n
  | otherwise = chainStop . squareDigits $ n

main :: IO ()
main = print . length . filter ((==89) . chainStop) $ [1..10^7]
