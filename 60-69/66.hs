import           Data.List
import           Data.Ord

next :: (Integral a) => a -> (a, a) -> (a, (a, a))
next n (x, y) =
  let denom = (n - x * x) `div` y
      multi = subtract 1 . head .
              dropWhile ((>0) . (\z -> sqrt (fromIntegral n) + fromIntegral z) . (x-) . (*denom))
              $ [1..]
  in (multi, (multi * denom - x, denom))

genSeq :: (Integral a) => a -> [(a, (a, a))]
genSeq n = (initElem, (initElem, 1)):unfoldr f (initElem, 1)
  where initElem = floor . sqrt . fromIntegral $ n
        f s = Just (res, snd res)
          where
            res = next n s

getUniq :: (Integral a) => a -> [a]
getUniq n = map fst $ take 2 list ++ takeWhile (/= hd) (tail subSeq)
  where
    list = genSeq n
    subSeq = tail list
    hd = head subSeq

coeffs :: (Integral a) => a -> [a]
coeffs n = let (hd:rest) = getUniq n
           in hd:cycle rest

fadd :: (Integral a) => (a, a) -> (a, a) -> (a, a)
fadd (n, _) (x, y) = (n * x + y, x)

valueAt :: (Integral a) => Int -> [a] -> (a, a)
valueAt n = foldr1 fadd . map (\x -> (x, 1)) . take n

minSol :: (Integral a) => a -> a
minSol n = fst . head . dropWhile f $ map (flip valueAt $ coeffs n) [1..]
  where
    f (x, y) = x^2 - n * y^2 /= 1

isSquare :: (Integral a) => a -> Bool
isSquare n = (==n) . (^2) . floor . sqrt . fromIntegral $ n

main :: IO ()
main = print . maximumBy (comparing minSol) . filter (not . isSquare) $ [2..1000]
