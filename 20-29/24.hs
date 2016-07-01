import           Data.List

n = 10
target = 1000000

fact :: Int -> Int
fact x = product [1..x]

listOfNum :: Int -> [Int]
listOfNum m = snd $ foldr f (m, []) [1..n-1]
  where
    f x (r, l) = let divisor = fact x
                     quotient = r `div` divisor
                     remain = r - quotient * divisor
                 in (remain, quotient:l)

getNumber :: Int -> Integer
getNumber m = read . fst $ foldr f ("", [0..9]) $ 0:listOfNum m
  where
    f x (s, l) = let d = l !! x
                     r = delete d l
                 in (s ++ show d, r)

main :: IO ()
main = print $ getNumber (target - 1)
