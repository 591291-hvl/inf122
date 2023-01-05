main :: IO ()
main = do
    ignore <- getLine
    line <- getLine
    print $ sum (map read (words line))