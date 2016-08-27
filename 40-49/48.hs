main :: IO ()
main = putStrLn . reverse . take 10 . reverse . show . sum . map (\x -> x^x) $ [1..1000]
