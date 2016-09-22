import           Data.Char
import           Data.Function
import           Data.List
import           Data.Ord

data Poker = Poker {
  value :: Int,
  suit  :: Int
} deriving (Ord, Eq, Show)

isLargerPokers :: [Poker] -> [Poker] -> Bool
isLargerPokers ps1 ps2
  | r1 == r2 = f ps1 > f ps2
  | otherwise  = r1 > r2
  where
    r1 = rank ps1
    r2 = rank ps2
    f = map value

rank :: [Poker] -> (Int, [Int])
rank ps = (f ps, map (value .  (!! 0) . nub) $ sortBy g groups)
  where
    f ps
      | flush ps && map value ps == [10,11,12,13,14] = 10
      | flush ps && consecutive ps = 9
      | fourKind = 8
      | fullHouse = 7
      | flush ps = 6
      | consecutive ps = 5
      | threeKind = 4
      | pairNum == 2 = 3
      | pairNum == 1 = 2
      | otherwise = 1
    flush = (==1) . length . nub . map suit
    consecutive ps = let vs = nub . map value $ ps
                     in length vs == 5 && head vs - last vs == 4
    groups = groupBy ((==) `on` value) $ ps
    lens = map length groups
    fourKind = (==4) . maximum $ lens
    fullHouse = (==[2,3]) . sort $ lens
    threeKind = (==3) . maximum $ lens
    pairNum = length . filter (==2) $ lens
    g x y
      | length x == length y = comparing head y x
      | otherwise = comparing length y x

readPoker :: String -> Poker
readPoker (v:s:_) = Poker (f v) (ord s)
  where
    f x
      | x == 'T' = 10
      | x == 'J' = 11
      | x == 'Q' = 12
      | x == 'K' = 13
      | x == 'A' = 14
      | otherwise = ord x - ord '1' + 1

main :: IO ()
main =
  do
    content <- readFile "p054_poker.txt"
    let plays = map (splitAt 5 . words) . lines $ content
        f = sortBy (flip $ comparing value) . map readPoker
        res = [isLargerPokers (f s1) (f s2) | (s1, s2) <- plays]
    print . length . filter id $ res
