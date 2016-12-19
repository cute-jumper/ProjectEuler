import           Data.List

isBouncy :: Int -> Bool
isBouncy n = not $ isIncreasing s || isIncreasing (reverse s)
  where
    s = show n

isIncreasing :: String -> Bool
isIncreasing n = fst $ foldl' (\(acc, m) y -> (acc && m <= y, y)) (True, '0') n

main :: IO ()
main = print . (1+) . length . takeWhile (\(x,y) -> x * 100 < y * 99) . tail . scanl f (0,0) $ [1..]
  where
    f (b, t) x = case isBouncy x of
                   True -> (b + 1, t + 1)
                   False -> (b, t + 1)
