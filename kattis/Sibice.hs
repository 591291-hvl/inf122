main :: IO()
main = do
    a <- getLine
    helper (map read(words a))

helper:: (Read a, Ord a, Floating a) => [a] -> IO()
helper (0:xs) = return ()
helper [x,y,z] = do
    a <- getLine
    case (read a) <= ( sqrt(y**2 + z**2)) of
        True -> putStrLn "DA"
        False -> putStrLn "NE"
    helper ((x-1):y:z:[])
