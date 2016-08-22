import           Control.Applicative

genPanta :: Int -> Int
genPanta n = n * (3 * n - 1) `div` 2

pantagonal :: [Int]
pantagonal = map genPanta [1..]

pantaDiff :: [(Int, Int)]
pantaDiff = zipWith (\x y -> (y, x - y)) (tail pantagonal) pantagonal

isPantagonal :: Int -> Bool
isPantagonal n = genPanta (floor ((sqrt (fromIntegral (24 * n + 1)) + 1) / 6)) == n

findOtherSmall :: Int -> Maybe (Int, Int)
findOtherSmall n = let lst = takeWhile (<n) pantagonal
                       res = [a | a <- lst, isPantagonal (a + n), isPantagonal (a + n + a)]
                   in
                     case res of
                       [] -> Nothing
                       (x:_) -> Just (x, x + n)

findOther :: Int -> Maybe (Int, Int)
findOther n = let lst = dropWhile (<=n) pantagonal
                  valids = map fst . takeWhile (\(x, y) -> x > n && y <= n) $ pantaDiff
                  res = [a | a <- valids, isPantagonal (a + n), isPantagonal (a + n + a)]
              in
                findOtherSmall n <|> case res of
                                       [] -> Nothing
                                       (x:_) -> Just (x, x + n)

main :: IO ()
main = let res = fmap (uncurry subtract) . head $
                 dropWhile (== Nothing) $ map findOther pantagonal
       in
         case res of
           Just x -> print x
           _ -> error "No solution"
