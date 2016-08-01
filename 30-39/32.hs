import           Control.Monad
import           Data.List
import qualified Data.Set      as S

ns = ['1'..'9']

-- a + b == c or c + 1, a + b + c = 9
getSlots :: [(Int, Int)]
getSlots = [(a, b) | a <- [1..4], b <- [a..4], a + b == 5]

comb :: (Eq a) => [a] -> Int -> [[a]]
comb xs k = recComb xs k []
  where
    recComb _ 0 r = [r]
    recComb [] k _ = []
    recComb (hd:rest) k r = (recComb rest (k - 1) (hd:r)) ++ (recComb rest k r)

isValidCombs :: (Int, Int, [Char]) -> Bool
isValidCombs (a, b, cs) = cs == sort (show (a * b))

getNumsForSlot :: (Int, Int) -> [Int]
getNumsForSlot (a, b) = do n1 <- comb ns a
                           n2 <- comb (ns \\ n1) b
                           p1 <- permutations n1
                           p2 <- permutations n2
                           let m1 = read p1
                               m2 = read p2
                           guard $ isValidCombs (m1, m2, sort $ (ns \\ n1) \\ n2)
                           return (m1 * m2)

getValidProducts :: [Int]
getValidProducts = S.toList . S.fromList . concat $ map getNumsForSlot getSlots

main :: IO ()
main = print . sum $ getValidProducts
