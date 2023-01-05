main :: IO ()
main =
    getLine >>= \a -> 
        let nmb = map read(words a) in 
        let x = head nmb in 
        let y = last nmb in
            helper x y


helper :: Int -> Int-> IO()
helper 0 y = putStrLn $ show y
helper n y =
    getLine >>= \a ->
    helper (n-1) y


-- main :: IO ()
-- main = do
--     a <- getLine
--     let nmb = map read(words a)
--     let x = head nmb
--     let y = last nmb
--     helper x
--     putStrLn $ show $ y


-- helper :: Int -> IO()
-- helper 0 = return ()
-- helper n = do
--     a <- getLine
--     helper (n-1)

