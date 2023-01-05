main :: IO ()
main = do
    line <- getLine
    print $ case line == [x | x <- line, y <- line, x == y] of
        True -> 1
        False -> 0