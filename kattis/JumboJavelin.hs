main :: IO ()
main = do
    n <- getLine
    helper (read n) 1

helper :: Int -> Int -> IO()
helper 0 acc = print acc
helper n acc = do
    line <- getLine
    helper (n-1) (acc+(read line) -1)