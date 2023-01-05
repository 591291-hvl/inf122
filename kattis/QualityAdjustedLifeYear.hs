main :: IO ()
main = do
    n <- getLine
    helper (read n) (0)

helper :: (Read a, Show a , Fractional a) => Int -> a -> IO ()
helper 0 acc = do putStrLn $ show $ acc
helper n acc = do
    line <- getLine
    let nmb = map read (words line)
    helper (n-1) (acc +(head nmb) * (last nmb))