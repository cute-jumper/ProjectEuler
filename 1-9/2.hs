let fibs = 1:2:(zipWith (+) (tail fibs) fibs) in sum . filter even $ takeWhile (<=4000000) fibs


