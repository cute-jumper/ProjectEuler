import           Data.List

triangle :: IO [[Int]]
triangle = do
  content <- readFile "p067_triangle.txt"
  let fLines = lines content
  return . map (map read . words) $ fLines

main :: IO ()
main = do
  tr <- triangle
  print . foldr1 f $ tr
  where
    f x acc = zipWith3 (\x y n -> max x y + n) acc (tail acc) x
