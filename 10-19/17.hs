import           Data.Char

smallNumToWords = ["one", "two", "three", "four", "five", "six", "seven",
                   "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                   "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
                   "nineteen"]

tensToWords = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy",
               "eighty", "ninety"]

numToWords num
  | 0 < num && num < 20 = smallNumToWords !! (num - 1)
  | 20 <= num && num < 100 = let digit = num `mod` 10
                            in tensToWords !! (num `div` 10 - 2) ++
                               if digit == 0 then "" else "-" ++ numToWords digit
  | 100 <= num && num < 1000 = let hundreds = num `div` 100
                                   remain = num `mod` 100
                               in smallNumToWords !! (hundreds - 1) ++ " hundred" ++
                                  if remain == 0 then "" else " and " ++ numToWords remain
  | num == 1000 = "one thousand"
  | otherwise = error "Not implemeted!"

main = do
   print . sum . map (length . filter isLetter . numToWords) $ [1..1000]
