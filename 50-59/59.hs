import           Data.Bits
import           Data.Char
import           Data.List

keys = [[a, b, c] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]

main :: IO ()
main =
  do
    content <- readFile "p059_cipher.txt"
    let cipher = read ("[" ++ content ++ "]")
        plain = [t | key <- keys, let t = map chr . zipWith xor (cycle (map ord key)) $ cipher,
                                      all isPrint t,
                                      "the " `isInfixOf` t]
    print . sum . map ord $ plain !! 0
