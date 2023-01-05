main :: IO ()
main = do
    line <- getLine
    putStrLn $ helper line

helper :: String -> String
helper [] = "no hiss"
helper [a] = "no hiss"
helper (a:b:c)
    | a == b && a == 's' = "hiss"
    | otherwise = helper (b:c)