import           Data.List

isValid :: Int -> Bool
isValid n = (length . nub $ map (sort . show . (n*)) [1..6]) == 1

main :: IO ()
main = print . head $ dropWhile (not . isValid) [1..]
