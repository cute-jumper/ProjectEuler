import           Control.Arrow
import           Data.Char

getLimit :: Int
getLimit = fst . head $ dropWhile (uncurry (>=)) $ map ((9^5*) &&& (10^)) [1..]

isFifthSame :: Int -> Bool
isFifthSame x = x == sum (map ((^5) . digitToInt) $ show x)

listOfFifthSameNums :: [Int]
listOfFifthSameNums = filter isFifthSame [2..getLimit-1]

main :: IO ()
main = print $ sum listOfFifthSameNums
