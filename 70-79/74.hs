import           Data.Char
import           Data.List

loopLength :: Int -> Int
loopLength a = f a []
  where
    f a b = case elemIndex a b of
              Just x -> length b
              _ -> f (getNext a) (a:b)

getNext :: Int -> Int
getNext a = sum . map ((\x -> product [2..x]) . digitToInt) . show $ a

main :: IO ()
main = print . length . filter (==60) . map loopLength $ [1..(10^6)]
