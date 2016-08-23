import           Data.List

triangles :: [Int]
triangles = map (\x -> x * (x + 1) `div` 2) [1..]

genPenta :: Int -> Int
genPenta n = n * (3 * n - 1) `div` 2

genHexa :: Int -> Int
genHexa n = n * (2 * n - 1)

isPenta :: Int -> Bool
isPenta n = genPenta (floor ((sqrt (fromIntegral (24 * n + 1)) + 1) / 6)) == n

isHexa :: Int -> Bool
isHexa n = genHexa (floor ((sqrt (fromIntegral (8 * n + 1)) + 1) / 4)) == n

main :: IO ()
main = print . head . filter (\x -> isPenta x && isHexa x) . drop 285 $ triangles
