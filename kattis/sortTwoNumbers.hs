main :: IO ()
main = 
    getLine >>= \a -> 
    helper (map read (words a))

helper :: [Int] -> IO()
helper [x,y] = case x > y of 
    True -> 
        putStrLn $ (show y) ++ " " ++ (show x)
    False -> 
        putStrLn $ (show x) ++ " " ++ (show y)


-- main :: IO ()
-- main = do
--     a <- getLine
--     helper (map read (words a))

-- helper :: [Int] -> IO()
-- helper [x,y] = case x > y of 
--     True -> do
--         putStrLn $ (show y) ++ " " ++ (show x)
--     False -> do
--         putStrLn $ (show x) ++ " " ++ (show y)


