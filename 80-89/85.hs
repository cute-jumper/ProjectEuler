import           Data.List
import           Data.Ord

main :: IO ()
main = print . minimumBy (comparing fst) $
  [(total, x * y) | x <- [1..100], y <- [1..100],
    let total = abs(x * (x + 1) * y * (y + 1) - 8 * 10^6)]
