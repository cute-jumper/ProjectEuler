import           Data.Char
import           Data.List

getSortedList :: IO [String]
getSortedList =
  do content <- readFile "p022_names.txt"
     return . sort . read $ "[" ++ content ++ "]"

getScore :: IO Int
getScore =
  do l <- getSortedList
     return . sum . zipWith f l $ [1..]
       where
         f x y = (*y) . sum . map (\z -> ord z - ord 'A' + 1) $ x

main :: IO ()
main = do x <- getScore
          print x
