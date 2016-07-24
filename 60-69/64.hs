import           Data.List

next :: Int -> (Int, Int) -> (Int, (Int, Int))
next n (x, y) =
  let denom = (n - x * x) `div` y
      multi = subtract 1 . head .
              dropWhile ((>0) . (\z -> sqrt (fromIntegral n) + fromIntegral z) . (x-) . (*denom))
              $ [1..]
  in (multi, (multi * denom - x, denom))

genSeq :: Int -> [(Int, (Int, Int))]
genSeq n = (initElem, (initElem, 1)):unfoldr f (initElem, 1)
  where initElem = floor . sqrt . fromIntegral $ n
        f s = Just (res, snd res)
          where
            res = next n s

getUniq :: Int -> [Int]
getUniq n = map fst $ take 2 list ++ takeWhile (/= hd) (tail subSeq)
  where
    list = genSeq n
    subSeq = tail list
    hd = head subSeq

period :: Int -> Int
period = subtract 1 . length . getUniq

isSquare :: Int -> Bool
isSquare n = (==n) . (^2) . floor . sqrt . fromIntegral $ n

main :: IO ()
main = print . length . filter odd . map period . filter (not . isSquare) $ [2..10000]
