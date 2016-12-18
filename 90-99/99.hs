import           Data.List
import           Data.Ord

comp :: (Float, Float) -> Float
comp (x, y) = log x * y

exps :: IO [(Float, Float)]
exps = do
  content <- readFile "base_exp.txt"
  let ls = lines content
  return . map (\l -> read $ "(" ++ l ++ ")") $ ls

main :: IO ()
main = do
  p <- exps
  print . snd . maximumBy (comparing $ comp . fst) $ zip p [1..]
