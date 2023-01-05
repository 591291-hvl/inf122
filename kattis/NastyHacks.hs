main :: IO()
main = do
    n <- getLine
    helper (read n)

helper :: Int -> IO()
helper 0 = return ()
helper n = do
    line <- getLine
    let nmb = map read (words line) :: [Int]
    putStrLn $ helperHelper nmb
    helper (n-1)

helperHelper:: [Int] -> String
helperHelper (x:y:z:_)  
    | y > x + z = "advertise"
    | y == x + z = "does not matter"
    | otherwise = "do not advertise"
