import           Data.List
import qualified Data.Map  as M

keys :: IO [String]
keys = do
  content <- readFile "p079_keylog.txt"
  return . nub . lines $ content

mappings :: IO (M.Map Char [Char])
mappings =
  do lst <- fmap (map (\ks -> zip ks (map return . tail $ ks))) keys
     return . M.fromListWith (\x y -> nub $ x ++ y) . concat $ lst

startPoints :: IO [Char]
startPoints =
  do
    ks <- fmap M.keys mappings
    ms <- mappings
    return $ M.fold (flip (\\)) ks ms

topSort :: M.Map Char [Char] -> [Char] -> String
topSort m [p] = f m p
  where
    f n y = case M.lookup y n of
              Just (x:_) ->
                let n1 = M.map ((delete x) . (delete y)) n
                    res = (f n1 x)
                in f (M.map (\z -> filter (not . (`elem` res)) z) n) y ++ res
              _ -> [y]
topSort m ps = concat [topSort m [p] | p <- ps]

main :: IO ()
main = do
  m <- mappings
  ps <- startPoints
  print $ topSort m ps
