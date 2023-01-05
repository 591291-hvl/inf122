main :: IO ()
main = do
    x <- getLine
    y <- getLine
    print $ helper (read x) (read y)

helper :: Int -> Int -> Int
helper x y 
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4