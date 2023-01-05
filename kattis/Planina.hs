main :: IO ()
main = do
    n <- getLine
    print $ helper (read n) 2

helper:: Int -> Int -> Int
helper 0 l = l*l
helper n l = helper (n-1) (l+l-1)