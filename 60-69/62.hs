import           Data.List

main :: IO ()
main = let cubes = map (sort . show . (^3)) $ [0..9000]
           res = head . head . filter ((==5) . length) . group . sort $ cubes
           Just idx = elemIndex res cubes
       in  print $ idx^3
