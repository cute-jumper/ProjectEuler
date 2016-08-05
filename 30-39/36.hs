import           Data.Char
import           Numeric

isPalindrom :: Int -> Bool
isPalindrom x = let dec = show x
                    bin = showIntAtBase 2 intToDigit x ""
                in dec == reverse dec && bin == reverse bin

getPalindroms :: [Int]
getPalindroms = filter isPalindrom [1..]

main :: IO ()
main = print . sum . takeWhile (<10^6) $ getPalindroms
