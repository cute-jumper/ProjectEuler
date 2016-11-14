import           Data.List

minPathSum :: [[Int]] -> Int
minPathSum (hd:rest) = last $ foldl' f (scanl1 (+) hd) rest
  where
    f acc x = tail . map snd $ scanl g (acc, head acc) x
    g (x:xs, left) y = (xs, y + min x left)

main :: IO ()
main =
  do
    content <- readFile "p081_matrix.txt"
    print . minPathSum $ [read ("[" ++ line ++ "]") | line <- lines content]
