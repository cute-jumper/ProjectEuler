import           Control.Arrow
import           Data.Char

fact :: Int -> Int
fact n = product [1..n]

getLimit :: Int
getLimit = fst . head . dropWhile (uncurry (>=)) $ map ((fact 9 *) &&& (10^)) [1..]

isCurious :: Int -> Bool
isCurious n = n == (sum . map (fact . digitToInt) $ show n)

getCuriousNums :: [Int]
getCuriousNums = filter isCurious [3..getLimit]

main :: IO ()
main = print $ sum getCuriousNums
