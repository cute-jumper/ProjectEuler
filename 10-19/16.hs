import           Data.Char

main = do
  print . sum . map digitToInt . show $ 2^1000
