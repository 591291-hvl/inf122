main :: IO()
main = do
    line <- getLine
    print $ case (line !! 0) == (line !! 1) && (line !! 1) == (line !! 2) && (line !! 2) == '5' of
        True -> 1
        False -> 0