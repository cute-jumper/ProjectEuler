isConcealed :: Integer -> Bool
isConcealed n = length ds == 17 && [ds !! k | k <- [0,2..16]] == ['1'..'9']
  where
    ds = show n

main :: IO ()
main = print . (*10) . head . dropWhile (not . isConcealed . (^2)) $ [100000003,100000013..35*10^7] ++ [100000007,100000017..35*10^7]
