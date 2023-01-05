main :: IO ()
main =
    getLine >>= \a ->
    helper (read a) 1

helper :: Int -> Int -> IO()
helper n a = case n < a of 
    True -> return ()
    False -> 
        putStrLn ((show a) ++" Abracadabra") >>
        helper n (a+1)

-- main :: IO ()
-- main = do
--     a <- getLine
--     helper (read a) 1

-- helper :: Int -> Int -> IO()
-- helper n a = case n < a of 
--     True -> return ()
--     False -> do
--         putStrLn ((show a) ++" Abracadabra")
--         helper n (a+1)