main :: IO()
main = do
    ignore <- getLine
    line <- getLine
    print $ (-1) * sum (filter (<=0) (map read (words line)))