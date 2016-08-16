import           Control.Monad
import           Data.Char
import           Data.List.Split

engWords :: IO [Int]
engWords = do
  content <- readFile "p042_words.txt"
  return . map f . splitOn "," $ content
    where
      f s = sum $ map (\x -> ord (toLower x) - ord 'a' + 1 ) ((read s)::String)

isTriangleNum :: Int -> Bool
isTriangleNum n = n1 * (n1 + 1) == n * 2
  where
    n1 = floor . sqrt . fromIntegral $ n * 2

main :: IO ()
main = do
  ws <- engWords
  print . length . mfilter isTriangleNum $ ws
