import           Data.List

isLeap::Int -> Bool
isLeap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0)

getDays::Int -> Int -> Int
getDays y m
  | isLeap y && m == 2 = 29
  | m == 2 = 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31

getNumOfFirstSundays::Int
getNumOfFirstSundays = snd $ foldl' f initValue [(y, m) | y <- [1901..2000], m <- [1..12]]
  where
    f (acc, count) (y, m) =
      let days = acc + getDays y m
      in
        if acc `mod` 7 == 6
        then (days, count + 1)
        else (days, count)
    initValue = (sum $ map (getDays 1900) [1..12], 0)
