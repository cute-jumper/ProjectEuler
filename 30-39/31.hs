ps = [1, 2, 5, 10, 20, 50, 100, 200]

recComp 0 p _ = [p]
recComp t p xs
  | t < 0 = []
  | otherwise = concat [recComp (t - h) (h:p) (filter (>= h) xs) | h <- xs]

getCombinations :: Int -> [[Int]]
getCombinations total = recComp total [] ps
