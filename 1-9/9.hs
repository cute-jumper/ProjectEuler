head [x * y * z | x <- [1..998], y <- [x, x+1..998], let z = 1000 - x - y, x^2 + y^2 == z^2]
