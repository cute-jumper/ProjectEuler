import           Data.Char
import           Data.List
import qualified Data.Set  as S

isPrime :: Int -> Bool
isPrime x
  | x < 2 = False
  | x == 2 = True
  | otherwise = let h = round . sqrt . fromIntegral $ x
            in all (\y -> x `mod` y /= 0) (2:[3,5..h])

getAllRots :: Int -> [Int]
getAllRots x = S.toList . S.fromList $ rotInt x
  where
    rotInt x = let xs = show x
               in [ read $ b ++ a | i <- [0..length xs - 1], let (a, b) = splitAt i xs]

isCircular :: [Int] -> Bool
isCircular xs = all isPrime xs

getListOfCircular :: [Int]
getListOfCircular = foldl' f [] $ [2..10^6]
  where
    f acc x
      | x `elem` acc = acc
      | otherwise = let rots = getAllRots x
                    in if isCircular rots
                       then rots ++ acc
                       else acc


main :: IO ()
main = print . length $ getListOfCircular
