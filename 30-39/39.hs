import Control.Arrow
import Data.List
import Data.Ord

getValidTuples :: Int -> [(Int, Int, Int)]
getValidTuples n = [ (a, b, c) | a <- [1..(n `div` 3)], b <- [a..(n `div` 2)],
                     let c = n - a - b, c > b, a + b > c, a^2 + b^2 == c^2]

main :: IO ()
main = print . snd . maximumBy (comparing fst) . map ((length . getValidTuples) &&& id) $ [3..1000]
