main :: IO()
main = do
    n <- getLine
    let ee = init $ tail n
    putStrLn $ [head n] ++ ee ++ ee ++ [last n]