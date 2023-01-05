main :: IO()
main = do
    line <- getLine
    case mod (read line) 2 /= 0 of 
        True -> putStrLn $ "first"
        False -> putStrLn $ "second"