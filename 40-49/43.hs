import           Data.List

mappings :: [Int]
mappings = [2, 3, 5, 7, 11, 13, 17]

isDivisible :: String -> Bool
isDivisible n = f (tail n) 0
  where
    l = length mappings
    f s x
      | x >= l = True
      | otherwise = (read . take 3 $ s) `mod` (mappings !! x) == 0 &&
                    f (tail s) (x + 1)

main :: IO ()
main = print . sum . map read . filter isDivisible . permutations $ ['0'..'9']
