isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome n = let s = show n
                 in s == reverse s

reverseAdd :: (Integral a, Show a, Read a) => a -> a
reverseAdd n = n + (read . reverse . show $ n)

isLychrel :: (Integral a, Show a, Read a) => a -> Bool
isLychrel n = f n 1
  where
    f n c
      | c > 50 = True
      | otherwise = let n1 = reverseAdd n
                    in
                      if isPalindrome n1
                      then False
                      else f n1 (c + 1)

main :: IO ()
main = print . length . filter isLychrel $ [1..9999]
