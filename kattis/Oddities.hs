main :: IO()
main = 
    getLine >>= \n ->
    helper (read n)


helper :: Int -> IO()
helper 0 = return ()
helper n = 
    getLine >>= \x ->
    putStrLn $ x ++ " is " ++ case mod ((read x) :: Int) 2 == 0 of 
        True ->  "even"
        False ->  "odd" >>
    helper (n-1)



-- main :: IO()
-- main = do
--     n <- getLine
--     helper (read n)


-- helper :: Int -> IO()
-- helper 0 = return ()
-- helper n = do
--     x <- getLine
--     putStrLn $ x ++ " is " ++ case mod ((read x) :: Int) 2 == 0 of 
--         True ->  "even"
--         False ->  "odd"
--     helper (n-1)