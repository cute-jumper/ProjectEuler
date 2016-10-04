import           Data.List

cf :: [(Integer, Integer)]
cf = unfoldr (\(x,y) -> let t = (x + 2 * y, x + y) in Just ((x, y), t)) (3, 2)


main :: IO ()
main = print . length . filter (\(x,y) -> length (show x) > length (show y)) . take 1000 $ cf
