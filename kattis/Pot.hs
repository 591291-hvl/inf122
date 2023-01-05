main :: IO ()
main =
    getLine >>= \n ->
    helper (read n) 0

helper ::(Show a, Read a, Floating a, RealFrac a) => Int -> a -> IO ()
helper 0 acc = print $ (round acc)
helper n acc =
    getLine >>= \line ->
    helper (n-1) (acc + (read (init line))**(read ([last line]))) 


-- main :: IO ()
-- main = do
--     n <- getLine
--     helper (read n) 0

-- helper ::(Show a, Read a, Floating a, RealFrac a) => Int -> a -> IO ()
-- helper 0 acc = print $ (round acc)
-- helper n acc = do
--     line <- getLine
--     helper (n-1) (acc + (read (init line))**(read ([last line]))) 