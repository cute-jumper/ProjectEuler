import Data.List

isPandigital :: String -> Bool
isPandigital n = sort n == ['1'..'9']

getLowerBound :: Int -> Int
getLowerBound n = 10 ^ (9 `div` n - 1)

getValidNumbers :: Int -> [Int]
getValidNumbers n = map read . filter isPandigital $
                    [ concatMap (\x -> show (x * l)) [1..n] | l <- [low..high]]
  where
    low = getLowerBound n
    high = 10 * low - 1

main :: IO ()
main = print . maximum . concatMap getValidNumbers $ [2..9]
