main :: IO ()
main = 
    getLine >>= \nmb ->
    getLine >>= \n ->
    helper (read nmb) (read n) (read nmb) >>= \a ->
    print a

-- main :: IO ()
-- main = do
--     nmb <- getLine
--     n <- getLine
--     a <- helper (read nmb) (read n) (read nmb)
--     print a

helper ::Int -> Int -> Int -> IO Int
helper _ 0 acc = return acc
helper nmb n acc =
    getLine >>= \a ->
    helper nmb (n-1) (acc + nmb - (read a))

    
-- helper ::Int -> Int -> Int -> IO Int
-- helper _ 0 acc = return acc
-- helper nmb n acc = do
--     a <- getLine
--     helper nmb (n-1) (acc + nmb - (read a))

