--Dont work :((, non ending recursion

main :: IO()
main =
    getLine >>= \w ->
    getLine >>= \n ->
    helper (read w) (read n) 0

helper :: Int -> Int -> Int -> IO ()
helper w 0 s = putStrLn $ show (div s w)
helper w n s =  
    getLine >>= \x ->
    let list = map read (words x) in
    helper w (n-1) (s + (head list * last list)) 


-- main :: IO()
-- main = do
--     w <- getLine
--     n <- getLine
--     helper (read w) (read n) 0

-- helper :: Int -> Int -> Int -> IO ()
-- helper w 0 s = putStrLn $ show (div s w)
-- helper w n s = do 
--     x <- getLine
--     let list = map read (words x)
--     helper w (n-1) (s + (head list * last list)) 